package models

import java.time.LocalDate
import slick.jdbc.H2Profile.api._
import slick.lifted.ProvenShape

case class Loan(
  id: Long = 0L,
  ownerId: Long,
  borrowerId: Long,
  bookItemId: Long,
  loanedAt: LocalDate,
  dueAt: LocalDate,
  returnedAt: Option[LocalDate],
  extensionsUsed: Int = 0,
  status: String
)

class LoansTable(tag: Tag) extends Table[Loan](tag, "LOANS") {
  def id: Rep[Long] = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def ownerId: Rep[Long] = column[Long]("OWNER_ID")
  def borrowerId: Rep[Long] = column[Long]("BORROWER_ID")
  def bookItemId: Rep[Long] = column[Long]("BOOK_ITEM_ID")
  def loanedAt: Rep[LocalDate] = column[LocalDate]("LOANED_AT")
  def dueAt: Rep[LocalDate] = column[LocalDate]("DUE_AT")
  def returnedAt: Rep[Option[LocalDate]] = column[Option[LocalDate]]("RETURNED_AT")
  def extensionsUsed: Rep[Int] = column[Int]("EXTENSIONS_USED")
  def status: Rep[String] = column[String]("STATUS")

  def owner = foreignKey("FK_LOAN_OWNER", ownerId, TableQuery[UsersTable])(_.id)
  def borrower = foreignKey("FK_LOAN_BORROWER", borrowerId, TableQuery[UsersTable])(_.id)
  def bookItem = foreignKey("FK_LOAN_BOOK_ITEM", bookItemId, TableQuery[BookItemsTable])(_.id)

  override def * : ProvenShape[Loan] = (id, ownerId, borrowerId, bookItemId, loanedAt, dueAt, returnedAt, extensionsUsed, status).mapTo[Loan]
}
