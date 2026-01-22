package models

import slick.jdbc.H2Profile.api._
import slick.lifted.ProvenShape

import scala.math.BigDecimal

case class User(id: Long = 0L, name: String, email: String, passwordHasher: String, passwordHash: String)

case class LibraryPolicy(
  id: Long = 0L,
  ownerId: Long,
  defaultLoanDays: Int = 7,
  maxExtensions: Int = 1,
  dailyOverdueFee: BigDecimal
)

class UsersTable(tag: Tag) extends Table[User](tag, "USERS") {
  def id: Rep[Long] = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def name: Rep[String] = column[String]("NAME")
  def email: Rep[String] = column[String]("EMAIL", O.Unique)
  def passwordHasher: Rep[String] = column[String]("PASSWORD_HASHER")
  def passwordHash: Rep[String] = column[String]("PASSWORD_HASH")

  override def * : ProvenShape[User] = (id, name, email, passwordHasher, passwordHash).mapTo[User]
}

class LibraryPoliciesTable(tag: Tag) extends Table[LibraryPolicy](tag, "LIBRARY_POLICIES") {
  def id: Rep[Long] = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def ownerId: Rep[Long] = column[Long]("OWNER_ID", O.Unique)
  def defaultLoanDays: Rep[Int] = column[Int]("DEFAULT_LOAN_DAYS")
  def maxExtensions: Rep[Int] = column[Int]("MAX_EXTENSIONS")
  def dailyOverdueFee: Rep[BigDecimal] = column[BigDecimal]("DAILY_OVERDUE_FEE")

  def owner = foreignKey("FK_LIBRARY_POLICY_OWNER", ownerId, TableQuery[UsersTable])(_.id)

  override def * : ProvenShape[LibraryPolicy] = (id, ownerId, defaultLoanDays, maxExtensions, dailyOverdueFee).mapTo[LibraryPolicy]
}
