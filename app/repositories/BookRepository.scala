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
  database.run((books.schema ++ bookItems.schema ++ loans.schema ++ libraryPolicies.schema).createIfNotExists)

  def list(): Future[Seq[Book]] = database.run(books.result)

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
