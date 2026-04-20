package repositories

import javax.inject.Inject
import models._
import play.api.Configuration
import slick.jdbc.H2Profile.api._
import slick.util.AsyncExecutor

import java.time.LocalDate
import scala.concurrent.{ExecutionContext, Future}

class BookRepository @Inject()(config: Configuration)(implicit ec: ExecutionContext) {

  private val dbThreads = config.getOptional[Int]("app.db.threads").getOrElse(10)
  private val executor = AsyncExecutor("book-db", numThreads = dbThreads, queueSize = 1000)
  private val database = Database.forURL(
    config.getOptional[String]("db.default.url").getOrElse("jdbc:h2:mem:play;DB_CLOSE_DELAY=-1"),
    driver = config.getOptional[String]("db.default.driver").getOrElse("org.h2.Driver"),
    executor = executor
  )
  private val books = TableQuery[BooksTable]
  private val bookItems = TableQuery[BookItemsTable]
  private val loans = TableQuery[LoansTable]
  private val likes = TableQuery[BookLikesTable]
  private val reposts = TableQuery[BookRepostsTable]
  private val bookmarks = TableQuery[BookBookmarksTable]
  private val comments = TableQuery[BookCommentsTable]
  private val reports = TableQuery[BookReportsTable]
  private val loanPayments = TableQuery[LoanPaymentsTable]

  val schemaCreation: Future[Unit] = {
    val createSchema = (books.schema ++ bookItems.schema ++ loans.schema ++ loanPayments.schema ++ likes.schema ++ reposts.schema ++ bookmarks.schema ++ comments.schema ++ reports.schema).createIfNotExists
    val addCreatedAtColumn =
      sqlu"""ALTER TABLE BOOKS ADD COLUMN IF NOT EXISTS CREATED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL"""
    val addReviewHeadlineColumn =
      sqlu"""ALTER TABLE BOOKS ADD COLUMN IF NOT EXISTS REVIEW_HEADLINE VARCHAR(255)"""
    val addReadingStatusColumn =
      sqlu"""ALTER TABLE BOOKS ADD COLUMN IF NOT EXISTS READING_STATUS VARCHAR(50) DEFAULT 'FINISHED' NOT NULL"""
    val addRatingColumn =
      sqlu"""ALTER TABLE BOOKS ADD COLUMN IF NOT EXISTS RATING INTEGER"""
    val addAccruedOverdueFee =
      sqlu"""ALTER TABLE LOANS ADD COLUMN IF NOT EXISTS ACCRUED_OVERDUE_FEE NUMERIC(38, 2) DEFAULT 0 NOT NULL"""
    val addOverdueFeePaidAmount =
      sqlu"""ALTER TABLE LOANS ADD COLUMN IF NOT EXISTS OVERDUE_FEE_PAID_AMOUNT NUMERIC(38, 2) DEFAULT 0 NOT NULL"""
    val addOverdueFeePaidAt =
      sqlu"""ALTER TABLE LOANS ADD COLUMN IF NOT EXISTS OVERDUE_FEE_PAID_AT DATE"""

    database.run((createSchema >> addCreatedAtColumn >> addReviewHeadlineColumn >> addReadingStatusColumn >> addRatingColumn >> addAccruedOverdueFee >> addOverdueFeePaidAmount >> addOverdueFeePaidAt).map(_ => ()).transactionally)
  }

  private def calculateOverdueFee(loan: Loan, dailyFee: BigDecimal, asOf: LocalDate): BigDecimal = {
    val effectiveEndDate = loan.returnedAt.getOrElse(asOf)
    val overdueDays =
      if (loan.dueAt.isBefore(effectiveEndDate)) java.time.temporal.ChronoUnit.DAYS.between(loan.dueAt, effectiveEndDate)
      else 0L
    loan.accruedOverdueFee.max(BigDecimal(overdueDays) * dailyFee)
  }

  def list(): Future[Seq[Book]] = database.run(books.sortBy(_.createdAt.desc).result)

  private def normalizeIsbn(isbn: String): String =
    isbn.toUpperCase.replaceAll("[^0-9X]", "")

  def primaryIsbn(book: Book): Option[String] =
    book.metadata.isbn13.orElse(book.metadata.isbn10).map(normalizeIsbn).filter(_.nonEmpty)

  def trending(limit: Int = 3): Future[Seq[Book]] = {
    val query = books.sortBy { book =>
      likes.filter(_.bookId === book.id).length.desc
    }.take(limit)
    database.run(query.result)
  }

  // Interaction Counts
  def getInteractionStats(bookId: Long): Future[(Int, Int, Int, Int)] = {
    val lF = database.run(likes.filter(_.bookId === bookId).length.result)
    val rF = database.run(reposts.filter(_.bookId === bookId).length.result)
    val bF = database.run(bookmarks.filter(_.bookId === bookId).length.result)
    val cF = database.run(comments.filter(_.bookId === bookId).length.result)
    for {
      l <- lF; r <- rF; b <- bF; c <- cF
    } yield (l, r, b, c)
  }

  def getUserInteractions(bookId: Long, userId: Long): Future[(Boolean, Boolean, Boolean)] = {
    val lF = database.run(likes.filter(x => x.bookId === bookId && x.userId === userId).exists.result)
    val rF = database.run(reposts.filter(x => x.bookId === bookId && x.userId === userId).exists.result)
    val bF = database.run(bookmarks.filter(x => x.bookId === bookId && x.userId === userId).exists.result)
    for {
      l <- lF; r <- rF; b <- bF
    } yield (l, r, b)
  }

  // Toggles
  def toggleLike(userId: Long, bookId: Long): Future[Boolean] = {
    val action = likes.filter(x => x.userId === userId && x.bookId === bookId).result.headOption.flatMap {
      case Some(_) => likes.filter(x => x.userId === userId && x.bookId === bookId).delete.map(_ => false)
      case None => (likes += BookLike(userId, bookId)).map(_ => true)
    }
    database.run(action.transactionally)
  }

  def toggleRepost(userId: Long, bookId: Long): Future[Boolean] = {
    val action = reposts.filter(x => x.userId === userId && x.bookId === bookId).result.headOption.flatMap {
      case Some(_) => reposts.filter(x => x.userId === userId && x.bookId === bookId).delete.map(_ => false)
      case None => (reposts += BookRepost(userId, bookId)).map(_ => true)
    }
    database.run(action.transactionally)
  }

  def toggleBookmark(userId: Long, bookId: Long): Future[Boolean] = {
    val action = bookmarks.filter(x => x.userId === userId && x.bookId === bookId).result.headOption.flatMap {
      case Some(_) => bookmarks.filter(x => x.userId === userId && x.bookId === bookId).delete.map(_ => false)
      case None => (bookmarks += BookBookmark(userId, bookId)).map(_ => true)
    }
    database.run(action.transactionally)
  }

  // Comments
  def addComment(userId: Long, bookId: Long, content: String, parentId: Option[Long] = None): Future[Long] = {
    database.run((comments returning comments.map(_.id)) += BookComment(userId = userId, bookId = bookId, content = content, parentId = parentId))
  }

  def listComments(bookId: Long): Future[Seq[(BookComment, User)]] = {
    val query = for {
      c <- comments.filter(_.bookId === bookId)
      u <- TableQuery[models.UsersTable] if c.userId === u.id
    } yield (c, u)
    database.run(query.sortBy(_._1.createdAt.desc).result)
  }

  def listBookmarks(userId: Long): Future[Seq[Book]] = {
    val query = for {
      b <- bookmarks.filter(_.userId === userId)
      book <- books if b.bookId === book.id
    } yield book
    database.run(query.result)
  }

  def insert(book: Book): Future[Long] = database.run((books returning books.map(_.id)) += book)

  def insertWithItem(book: Book, item: models.BookItem): Future[Long] = {
    val action = for {
      bookId <- (books returning books.map(_.id)) += book
      _ <- bookItems += item.copy(bookId = bookId)
    } yield bookId
    database.run(action.transactionally)
  }

  def findById(bookId: Long): Future[Option[Book]] =
    database.run(books.filter(_.id === bookId).result.headOption)

  def listByIsbn(isbn: String): Future[Seq[Book]] = {
    val normalizedIsbn = normalizeIsbn(isbn)
    list().map(_.filter(book => primaryIsbn(book).contains(normalizedIsbn)))
  }

  def listRecentIsbnCommunities(limit: Int = 6): Future[Seq[(String, Seq[Book])]] =
    list().map(
      _.flatMap(book => primaryIsbn(book).map(_ -> book))
        .groupBy(_._1)
        .toSeq
        .sortBy { case (_, entries) => entries.map(_._2.createdAt).max }(using Ordering[java.time.LocalDateTime].reverse)
        .take(limit)
        .map { case (isbn, entries) => isbn -> entries.map(_._2).sortBy(_.createdAt)(using Ordering[java.time.LocalDateTime].reverse) }
    )

  def update(bookId: Long, book: Book): Future[Int] =
    database.run(books.filter(_.id === bookId).update(book.copy(id = bookId)))

  def findItemsByBookId(bookId: Long): Future[Seq[BookItem]] =
    database.run(bookItems.filter(_.bookId === bookId).result)

  def findItemsByOwner(ownerId: Long): Future[Seq[BookItem]] = {
    val query = for {
      item <- bookItems
      book <- books if book.id === item.bookId && book.ownerId === ownerId
    } yield item
    database.run(query.result)
  }

  def findItemById(itemId: Long): Future[Option[BookItem]] =
    database.run(bookItems.filter(_.id === itemId).result.headOption)

  def findLoanById(loanId: Long): Future[Option[Loan]] =
    database.run(loans.filter(_.id === loanId).result.headOption)

  def addItem(bookId: Long, barcode: String, callNumber: Option[String], location: Option[String]): Future[Long] = {
    val item = BookItem(
      bookId = bookId,
      barcode = barcode,
      callNumber = callNumber,
      location = location,
      status = "AVAILABLE",
      acquiredAt = Some(LocalDate.now())
    )
    database.run((bookItems returning bookItems.map(_.id)) += item)
  }

  def updateItemStatus(itemId: Long, status: String): Future[Int] =
    database.run(bookItems.filter(_.id === itemId).map(_.status).update(status))

  def listLoansByOwner(ownerId: Long): Future[Seq[Loan]] =
    database.run(loans.filter(_.ownerId === ownerId).result)

  def listLoansByBorrower(borrowerId: Long): Future[Seq[Loan]] =
    database.run(loans.filter(_.borrowerId === borrowerId).result)

  private def toLoanViewData(
    row: (Loan, BookItem, Book, User, User, LibraryPolicy),
    today: LocalDate
  ): LoanViewData = {
    val (loan, item, book, owner, borrower, policy) = row
    val effectiveEndDate = loan.returnedAt.getOrElse(today)
    val overdueDays =
      if (loan.dueAt.isBefore(effectiveEndDate)) java.time.temporal.ChronoUnit.DAYS.between(loan.dueAt, effectiveEndDate)
      else 0L
    val overdueFee = calculateOverdueFee(loan, policy.dailyOverdueFee, today)
    val outstandingOverdueFee = (overdueFee - loan.overdueFeePaidAmount).max(BigDecimal(0))
    LoanViewData(
      loan = loan,
      item = item,
      book = book,
      owner = owner,
      borrower = borrower,
      overdueDays = overdueDays,
      overdueFee = overdueFee,
      outstandingOverdueFee = outstandingOverdueFee,
      canExtend = loan.status == "ACTIVE" && loan.extensionsUsed < policy.maxExtensions,
      canSettleOverdueFee = outstandingOverdueFee > 0,
      isOverdueSettled = overdueFee > 0 && outstandingOverdueFee == 0
    )
  }

  def listLoanViewDataByOwner(ownerId: Long, today: LocalDate = LocalDate.now()): Future[Seq[LoanViewData]] = {
    val query = for {
      loan <- loans if loan.ownerId === ownerId
      item <- bookItems if item.id === loan.bookItemId
      book <- books if book.id === item.bookId
      owner <- TableQuery[UsersTable] if owner.id === loan.ownerId
      borrower <- TableQuery[UsersTable] if borrower.id === loan.borrowerId
      policy <- TableQuery[LibraryPoliciesTable] if policy.ownerId === loan.ownerId
    } yield (loan, item, book, owner, borrower, policy)

    database.run(query.sortBy(_._1.loanedAt.desc).result).map(_.map(toLoanViewData(_, today)))
  }

  def listLoanViewDataByBorrower(borrowerId: Long, today: LocalDate = LocalDate.now()): Future[Seq[LoanViewData]] = {
    val query = for {
      loan <- loans if loan.borrowerId === borrowerId
      item <- bookItems if item.id === loan.bookItemId
      book <- books if book.id === item.bookId
      owner <- TableQuery[UsersTable] if owner.id === loan.ownerId
      borrower <- TableQuery[UsersTable] if borrower.id === loan.borrowerId
      policy <- TableQuery[LibraryPoliciesTable] if policy.ownerId === loan.ownerId
    } yield (loan, item, book, owner, borrower, policy)

    database.run(query.sortBy(_._1.loanedAt.desc).result).map(_.map(toLoanViewData(_, today)))
  }

  def listLoanPaymentsByLoanIds(loanIds: Seq[Long]): Future[Map[Long, Seq[LoanPayment]]] =
    if (loanIds.isEmpty) Future.successful(Map.empty)
    else {
      database.run(loanPayments.filter(_.loanId.inSet(loanIds)).sortBy(_.paidAt.desc).result).map { payments =>
        payments.groupBy(_.loanId)
      }
    }

  def findLoanPaymentByReceiptNumber(receiptNumber: String): Future[Option[LoanPayment]] =
    database.run(loanPayments.filter(_.receiptNumber === receiptNumber).result.headOption)

  def insertLoan(loan: Loan): Future[Long] =
    database.run((loans returning loans.map(_.id)) += loan)

  def returnLoan(loanId: Long, returnedAt: LocalDate, status: String): Future[Int] =
    database.run(
      loans.filter(_.id === loanId)
        .map(loan => (loan.returnedAt, loan.status))
        .update((Some(returnedAt), status))
    )

  def returnLoanAndUpdateItemStatus(loanId: Long, returnedAt: LocalDate, status: String, itemStatus: String): Future[Boolean] = {
    val action = loans.filter(_.id === loanId).result.headOption.flatMap {
      case Some(loan) =>
        val overdueDays =
          if (loan.dueAt.isBefore(returnedAt)) java.time.temporal.ChronoUnit.DAYS.between(loan.dueAt, returnedAt)
          else 0L
        val accruedFeeAction = TableQuery[LibraryPoliciesTable].filter(_.ownerId === loan.ownerId).map(_.dailyOverdueFee).result.headOption.map {
          case Some(dailyFee) => BigDecimal(overdueDays) * dailyFee
          case None => BigDecimal(0)
        }
        for {
          accruedFee <- accruedFeeAction
          updatedLoans <- loans.filter(_.id === loanId)
            .map(existing => (existing.returnedAt, existing.status, existing.accruedOverdueFee))
            .update((Some(returnedAt), status, loan.accruedOverdueFee.max(accruedFee)))
          updatedItems <- bookItems.filter(_.id === loan.bookItemId)
            .map(_.status)
            .update(itemStatus)
        } yield updatedLoans > 0 && updatedItems > 0
      case None =>
        DBIO.successful(false)
    }

    database.run(action.transactionally)
  }

  def extendLoan(loanId: Long, extensionDays: Int, maxExtensions: Int): Future[Boolean] = {
    val action = loans.filter(_.id === loanId).result.headOption.flatMap {
      case Some(loan) if loan.status == "ACTIVE" && loan.extensionsUsed < maxExtensions =>
        loans.filter(_.id === loanId)
          .map(existing => (existing.dueAt, existing.extensionsUsed))
          .update((loan.dueAt.plusDays(extensionDays.toLong), loan.extensionsUsed + 1))
          .map(_ > 0)
      case _ =>
        DBIO.successful(false)
    }

    database.run(action.transactionally)
  }

  def settleOverdueFee(loanId: Long, paidAt: LocalDate): Future[Boolean] = {
    val action = loans.filter(_.id === loanId).result.headOption.flatMap {
      case Some(loan) =>
        TableQuery[LibraryPoliciesTable].filter(_.ownerId === loan.ownerId).map(_.dailyOverdueFee).result.headOption.flatMap {
          case Some(dailyFee) =>
            val totalFee = calculateOverdueFee(loan, dailyFee, paidAt)
            val outstanding = (totalFee - loan.overdueFeePaidAmount).max(BigDecimal(0))
            if (outstanding <= 0) DBIO.successful(false)
            else {
              val receiptNumber = s"RCPT-$loanId-${paidAt.toString.replace("-", "")}-${System.currentTimeMillis()}"
              for {
                updated <- loans.filter(_.id === loanId)
                  .map(existing => (existing.accruedOverdueFee, existing.overdueFeePaidAmount, existing.overdueFeePaidAt))
                  .update((totalFee, totalFee, Some(paidAt)))
                _ <- loanPayments += LoanPayment(loanId = loanId, amount = outstanding, paidAt = paidAt, receiptNumber = receiptNumber)
              } yield updated > 0
            }
          case None =>
            DBIO.successful(false)
        }
      case None =>
        DBIO.successful(false)
    }

    database.run(action.transactionally)
  }

  def payOverdueFee(loanId: Long, amount: BigDecimal, paidAt: LocalDate): Future[Boolean] = {
    val normalizedAmount = amount.setScale(2, BigDecimal.RoundingMode.HALF_UP)
    val action = loans.filter(_.id === loanId).result.headOption.flatMap {
      case Some(loan) if normalizedAmount > 0 =>
        TableQuery[LibraryPoliciesTable].filter(_.ownerId === loan.ownerId).map(_.dailyOverdueFee).result.headOption.flatMap {
          case Some(dailyFee) =>
            val totalFee = calculateOverdueFee(loan, dailyFee, paidAt)
            val outstanding = (totalFee - loan.overdueFeePaidAmount).max(BigDecimal(0))
            if (outstanding <= 0 || normalizedAmount > outstanding) {
              DBIO.successful(false)
            } else {
              val newPaidAmount = loan.overdueFeePaidAmount + normalizedAmount
              val isSettled = newPaidAmount >= totalFee
              val receiptNumber = s"RCPT-$loanId-${paidAt.toString.replace("-", "")}-${System.currentTimeMillis()}"
              for {
                updated <- loans.filter(_.id === loanId)
                  .map(existing => (existing.accruedOverdueFee, existing.overdueFeePaidAmount, existing.overdueFeePaidAt))
                  .update((totalFee, newPaidAmount, if (isSettled) Some(paidAt) else loan.overdueFeePaidAt))
                _ <- loanPayments += LoanPayment(loanId = loanId, amount = normalizedAmount, paidAt = paidAt, receiptNumber = receiptNumber)
              } yield updated > 0
            }
          case None =>
            DBIO.successful(false)
        }
      case _ =>
        DBIO.successful(false)
    }

    database.run(action.transactionally)
  }

  def createReport(userId: Long, bookId: Long, reason: String): Future[Long] =
    database.run((reports returning reports.map(_.id)) += BookReport(userId = userId, bookId = bookId, reason = reason))

  def countReportsForBook(bookId: Long): Future[Int] =
    database.run(reports.filter(_.bookId === bookId).length.result)
}
