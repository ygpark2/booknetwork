package repositories

import javax.inject.Inject
import models.{LibraryPolicy, User}
import play.api.Configuration
import slick.jdbc.H2Profile.api._
import slick.util.AsyncExecutor

import scala.concurrent.{ExecutionContext, Future}

class UserRepository @Inject()(config: Configuration)(implicit ec: ExecutionContext) {

  private val executor = AsyncExecutor("user-db", numThreads = 10, queueSize = 1000)
  private val database = Database.forURL(
    config.getOptional[String]("db.default.url").getOrElse("jdbc:h2:mem:play;DB_CLOSE_DELAY=-1"),
    driver = config.getOptional[String]("db.default.driver").getOrElse("org.h2.Driver"),
    executor = executor
  )

  private val users = TableQuery[models.UsersTable]
  private val libraryPolicies = TableQuery[models.LibraryPoliciesTable]
  database.run((users.schema ++ libraryPolicies.schema).createIfNotExists)

  def findByEmail(email: String): Future[Option[User]] =
    database.run(users.filter(_.email === email).result.headOption)

  def insert(user: User): Future[Long] =
    database.run((users returning users.map(_.id)) += user)

  def insertLibraryPolicy(policy: LibraryPolicy): Future[Long] =
    database.run((libraryPolicies returning libraryPolicies.map(_.id)) += policy)

  def findPolicyByOwner(ownerId: Long): Future[Option[LibraryPolicy]] =
    database.run(libraryPolicies.filter(_.ownerId === ownerId).result.headOption)

  def upsertPolicy(ownerId: Long, defaultLoanDays: Int, maxExtensions: Int, dailyOverdueFee: BigDecimal): Future[Long] = {
    val policy = LibraryPolicy(
      ownerId = ownerId,
      defaultLoanDays = defaultLoanDays,
      maxExtensions = maxExtensions,
      dailyOverdueFee = dailyOverdueFee
    )
    val action = libraryPolicies.filter(_.ownerId === ownerId).result.headOption.flatMap {
      case Some(existing) =>
        libraryPolicies.filter(_.ownerId === ownerId)
          .map(p => (p.defaultLoanDays, p.maxExtensions, p.dailyOverdueFee))
          .update((defaultLoanDays, maxExtensions, dailyOverdueFee))
          .map(_ => existing.id)
      case None =>
        (libraryPolicies returning libraryPolicies.map(_.id)) += policy
    }
    database.run(action.transactionally)
  }

  def count(): Future[Int] =
    database.run(users.length.result)
}
