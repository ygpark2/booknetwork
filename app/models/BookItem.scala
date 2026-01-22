package models

import java.time.LocalDate
import slick.jdbc.H2Profile.api._
import slick.lifted.ProvenShape

case class BookItem(
  id: Long = 0L,
  bookId: Long,
  barcode: String,
  callNumber: Option[String] = None,
  location: Option[String] = None,
  status: String = "AVAILABLE",
  acquiredAt: Option[LocalDate] = None,
  price: Option[BigDecimal] = None,
  conditionNote: Option[String] = None
)

class BookItemsTable(tag: Tag) extends Table[BookItem](tag, "BOOK_ITEMS") {
  def id: Rep[Long] = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def bookId: Rep[Long] = column[Long]("BOOK_ID")
  def barcode: Rep[String] = column[String]("BARCODE")
  def callNumber: Rep[Option[String]] = column[Option[String]]("CALL_NUMBER")
  def location: Rep[Option[String]] = column[Option[String]]("LOCATION")
  def status: Rep[String] = column[String]("STATUS")
  def acquiredAt: Rep[Option[LocalDate]] = column[Option[LocalDate]]("ACQUIRED_AT")
  def price: Rep[Option[BigDecimal]] = column[Option[BigDecimal]]("PRICE")
  def conditionNote: Rep[Option[String]] = column[Option[String]]("CONDITION_NOTE")

  def book = foreignKey("FK_BOOK_ITEM_BOOK", bookId, TableQuery[BooksTable])(_.id)

  override def * : ProvenShape[BookItem] = (id, bookId, barcode, callNumber, location, status, acquiredAt, price, conditionNote).mapTo[BookItem]
}
