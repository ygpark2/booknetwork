package repositories

import javax.inject.Inject
import models.{LibraryPolicy, User}
import play.api.Configuration
import slick.jdbc.H2Profile.api._
import slick.util.AsyncExecutor

import scala.concurrent.{ExecutionContext, Future}

class UserRepository @Inject()(config: Configuration)(implicit ec: ExecutionContext) {

  private val dbThreads = config.getOptional[Int]("app.db.threads").getOrElse(10)
  private val executor = AsyncExecutor("user-db", numThreads = dbThreads, queueSize = 1000)
  private val database = Database.forURL(
    config.getOptional[String]("db.default.url").getOrElse("jdbc:h2:mem:play;DB_CLOSE_DELAY=-1"),
    driver = config.getOptional[String]("db.default.driver").getOrElse("org.h2.Driver"),
    executor = executor
  )

  private val users = TableQuery[models.UsersTable]
  private val libraryPolicies = TableQuery[models.LibraryPoliciesTable]
  private val follows = TableQuery[models.FollowsTable]
  val schemaCreation: Future[Unit] = database.run((users.schema ++ libraryPolicies.schema ++ follows.schema).createIfNotExists)

  def findByEmail(email: String): Future[Option[User]] =
    database.run(users.filter(_.email === email).result.headOption)

  def findById(id: Long): Future[Option[User]] =
    database.run(users.filter(_.id === id).result.headOption)

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

  def listRecommendUsers(excludeUserId: Option[Long], limit: Int = 3): Future[Seq[User]] = {
    val query = excludeUserId match {
      case Some(userId) => users.filter(_.id =!= userId).take(limit)
      case None => users.take(limit)
    }
    database.run(query.result)
  }

  def follow(followerId: Long, followedId: Long): Future[Int] = {
    val action = follows.filter(f => f.followerId === followerId && f.followedId === followedId).exists.result.flatMap {
      case true => DBIO.successful(0)
      case false => follows += models.Follow(followerId, followedId)
    }
    database.run(action.transactionally)
  }

  def unfollow(followerId: Long, followedId: Long): Future[Int] = {
    val action = follows.filter(f => f.followerId === followerId && f.followedId === followedId).delete
    database.run(action)
  }

  def isFollowing(followerId: Long, followedId: Long): Future[Boolean] = {
    database.run(follows.filter(f => f.followerId === followerId && f.followedId === followedId).exists.result)
  }

  def count(): Future[Int] =
    database.run(users.length.result)
}
