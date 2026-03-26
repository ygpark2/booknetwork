package tasks

import javax.inject._
import models._
import repositories._
import scala.concurrent.{ExecutionContext, Future}
import java.time.LocalDate
import play.api.{Logger, Environment}
import scala.io.Source

@Singleton
class DataInitializer @Inject()(
  bookRepository: BookRepository,
  userRepository: UserRepository,
  env: Environment
)(implicit ec: ExecutionContext) {

  private val logger = Logger(this.getClass)
  private val systemEmail = "system@booknetwork.dev"

  val init: Future[Unit] = (for {
    _ <- userRepository.schemaCreation
    _ <- bookRepository.schemaCreation
    count <- userRepository.count()
    _ <- if (count == 0) {
      logger.info("No users found. Seeding initial data from CSV...")
      seedData()
    } else {
      Future.successful(())
    }
  } yield ()).recover {
    case e: Exception =>
      logger.error("Error during data initialization", e)
  }

  private def seedData(): Future[Unit] = {
    val systemUser = User(name = "System", email = systemEmail, passwordHasher = "bcrypt", passwordHash = "")
    val pandaUser = User(name = "PandaDEV", email = "panda@dev.com", passwordHasher = "bcrypt", passwordHash = "")
    val triUser = User(name = "tri", email = "tri@fevnem.com", passwordHasher = "bcrypt", passwordHash = "")
    val antonioUser = User(name = "Antônio \"acdc\" Jr.", email = "acdc@junior.com", passwordHasher = "bcrypt", passwordHash = "")

    for {
      userId <- userRepository.insert(systemUser)
      _ <- userRepository.insert(pandaUser)
      _ <- userRepository.insert(triUser)
      _ <- userRepository.insert(antonioUser)
      _ <- userRepository.insertLibraryPolicy(LibraryPolicy(ownerId = userId, dailyOverdueFee = BigDecimal(0.50)))
      books = loadBooksFromCsv(userId)
      _ <- Future.sequence(books.map(bookRepository.insert))
    } yield {
      logger.info(s"Successfully seeded ${books.length} books and dummy users from CSV.")
      ()
    }
  }

  private def loadBooksFromCsv(ownerId: Long): Seq[Book] = {
    env.getExistingFile("conf/seed-data.csv") match {
      case Some(file) =>
        val source = Source.fromFile(file, "UTF-8")
        try {
          source.getLines().drop(1).flatMap {
            line =>
              line.split(";") match {
                case Array(title, author, description) =>
                  Some(Book(ownerId = ownerId, title = title.trim, author = author.trim, description = Some(description.trim)))
                case _ =>
                  logger.warn(s"Invalid CSV line format: $line")
                  None
              }
          }.toSeq
        } finally {
          source.close()
        }
      case None =>
        logger.error("Seed data file not found at conf/seed-data.csv")
        Seq.empty
    }
  }
}