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
  status: String,
  accruedOverdueFee: BigDecimal = BigDecimal(0),
  overdueFeePaidAmount: BigDecimal = BigDecimal(0),
  overdueFeePaidAt: Option[LocalDate] = None
)

case class LoanViewData(
  loan: Loan,
  item: BookItem,
  book: Book,
  owner: User,
  borrower: User,
  overdueDays: Long,
  overdueFee: BigDecimal,
  outstandingOverdueFee: BigDecimal,
  canExtend: Boolean,
  canSettleOverdueFee: Boolean,
  isOverdueSettled: Boolean
)

case class LoanPayment(
  id: Long = 0L,
  loanId: Long,
  amount: BigDecimal,
  paidAt: LocalDate,
  receiptNumber: String
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
  def accruedOverdueFee: Rep[BigDecimal] = column[BigDecimal]("ACCRUED_OVERDUE_FEE")
  def overdueFeePaidAmount: Rep[BigDecimal] = column[BigDecimal]("OVERDUE_FEE_PAID_AMOUNT")
  def overdueFeePaidAt: Rep[Option[LocalDate]] = column[Option[LocalDate]]("OVERDUE_FEE_PAID_AT")

  def owner = foreignKey("FK_LOAN_OWNER", ownerId, TableQuery[UsersTable])(_.id)
  def borrower = foreignKey("FK_LOAN_BORROWER", borrowerId, TableQuery[UsersTable])(_.id)
  def bookItem = foreignKey("FK_LOAN_BOOK_ITEM", bookItemId, TableQuery[BookItemsTable])(_.id)

  override def * : ProvenShape[Loan] = (id, ownerId, borrowerId, bookItemId, loanedAt, dueAt, returnedAt, extensionsUsed, status, accruedOverdueFee, overdueFeePaidAmount, overdueFeePaidAt).mapTo[Loan]
}

class LoanPaymentsTable(tag: Tag) extends Table[LoanPayment](tag, "LOAN_PAYMENTS") {
  def id: Rep[Long] = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def loanId: Rep[Long] = column[Long]("LOAN_ID")
  def amount: Rep[BigDecimal] = column[BigDecimal]("AMOUNT")
  def paidAt: Rep[LocalDate] = column[LocalDate]("PAID_AT")
  def receiptNumber: Rep[String] = column[String]("RECEIPT_NUMBER", O.Unique)

  def loan = foreignKey("FK_LOAN_PAYMENT_LOAN", loanId, TableQuery[LoansTable])(_.id)

  override def * : ProvenShape[LoanPayment] = (id, loanId, amount, paidAt, receiptNumber).mapTo[LoanPayment]
}
