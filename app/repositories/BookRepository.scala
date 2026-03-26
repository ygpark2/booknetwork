package repositories

import javax.inject.Inject
import models._
import play.api.Configuration
import slick.jdbc.H2Profile.api._
import slick.util.AsyncExecutor

import java.time.LocalDate
import scala.concurrent.{ExecutionContext, Future}

class BookRepository @Inject()(config: Configuration)(implicit ec: ExecutionContext) {

  private val executor = AsyncExecutor("book-db", numThreads = 10, queueSize = 1000)
  private val database = Database.forURL(
    config.getOptional[String]("db.default.url").getOrElse("jdbc:h2:mem:play;DB_CLOSE_DELAY=-1"),
    driver = config.getOptional[String]("db.default.driver").getOrElse("org.h2.Driver"),
    executor = executor
  )
  private val books = TableQuery[BooksTable]
  private val bookItems = TableQuery[BookItemsTable]
  private val loans = TableQuery[LoansTable]
  private val libraryPolicies = TableQuery[LibraryPoliciesTable]
  private val likes = TableQuery[BookLikesTable]
  private val reposts = TableQuery[BookRepostsTable]
  private val bookmarks = TableQuery[BookBookmarksTable]
  private val comments = TableQuery[BookCommentsTable]

  val schemaCreation: Future[Unit] = database.run((books.schema ++ bookItems.schema ++ loans.schema ++ libraryPolicies.schema ++ likes.schema ++ reposts.schema ++ bookmarks.schema ++ comments.schema).createIfNotExists)

  def list(): Future[Seq[Book]] = database.run(books.result)

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

  def insertLoan(loan: Loan): Future[Long] =
    database.run((loans returning loans.map(_.id)) += loan)

  def returnLoan(loanId: Long, returnedAt: LocalDate, status: String): Future[Int] =
    database.run(
      loans.filter(_.id === loanId)
        .map(loan => (loan.returnedAt, loan.status))
        .update((Some(returnedAt), status))
    )
}
