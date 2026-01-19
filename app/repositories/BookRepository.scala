package repositories

import javax.inject.Inject
import models._
import play.api.db.Database
import slick.jdbc.H2Profile.api._
import slick.util.AsyncExecutor

import scala.concurrent.{ExecutionContext, Future}

class BookRepository @Inject()(db: Database)(implicit ec: ExecutionContext) {

  private val executor = AsyncExecutor("book-db", numThreads = 10, queueSize = 1000)
  private val database = Database.forDataSource(db.dataSource, None, executor, keepAliveConnection = false)
  private val books = TableQuery[BooksTable]

  def list(): Future[Seq[Book]] = database.run(books.result)

  def insert(book: Book): Future[Long] = database.run((books returning books.map(_.id)) += book)
}
