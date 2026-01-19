package models

import slick.jdbc.H2Profile.api._
import play.api.db.slick._
import java.util.Date
import java.sql.{ Date => SqlDate }
// import play.api._
// import play.api.Play.current
// import play.api.libs.json._
import slick.lifted.Tag
import views.html.helper._
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Lang

case class Loan(id: Option[Int] = None, userId: Int, bookId: Int, loanDate: java.time.LocalDateTime, dueDate: java.time.LocalDateTime, returnDate: Option[java.time.LocalDateTime], status: String) extends Entity

/** Table description of table COMPANY. Objects of this class serve as prototypes for rows in queries. */
abstract class LoansTable(tag: Tag) extends Table[Loan](tag, "LOAN") with TableBase[Loan] {
  def * = (id.?, userId, bookId, loanDate, dueDate, returnDate, status) <> (Loan.tupled, Loan.unapply)
  /** Maps whole row to an option. Useful for outer joins. */
  def ? = (id.?, userId.?, bookId.?, loanDate.?, dueDate.?, returnDate, status.?).shaped.<>({r=>import r._; _1.map(_=> Loan.tupled((_1, _2.get, _3.get, _4.get, _5.get, _6, _7.get)))}, (_:Any) =>
    throw new Exception("Inserting into ? projection not supported."))

  /** Database columns */
  val userId: Column[Int] = column[Int]("USER_ID")
  val bookId: Column[Int] = column[Int]("BOOK_ID")
  val loanDate: Column[java.time.LocalDateTime] = column[java.time.LocalDateTime]("LOAN_DATE")
  val dueDate: Column[java.time.LocalDateTime] = column[java.time.LocalDateTime]("DUE_DATE")
  val returnDate: Column[Option[java.time.LocalDateTime]] = column[Option[java.time.LocalDateTime]]("RETURN_DATE")
  val status: Column[String] = column[String]("STATUS", O.Default("ACTIVE"))

  /** Database column ID AutoInc, PrimaryKey */
  val id: Column[Int] = column[Int]("ID", O.AutoInc, O.PrimaryKey)

  def tinyDescription = s"Loan ${id}"

}
class Loans(tag: Tag) extends LoansTable(tag)

class LoanModel extends Model[Loan]{
  val playForm = Form(
    mapping(
      "id" -> optional(number),
      "userId" -> number,
      "bookId" -> number,
      "loanDate" -> localDateTime,
      "dueDate" -> localDateTime,
      "returnDate" -> optional(localDateTime),
      "status" -> nonEmptyText
    )(Loan.apply)(Loan.unapply)
  )
  def form(playForm: Form[Loan]) = LoanForm(playForm=playForm)

  class LoanLabels extends super.Labels {
    def singular = "Loan".toLowerCase
    def plural   = "Loans".toLowerCase
    object columns {
      def id: String = "Id"
      def userId: String = "User ID"
      def bookId: String = "Book ID"
      def loanDate: String = "Loan Date"
      def dueDate: String = "Due Date"
      def returnDate: String = "Return Date"
      def status: String = "Status"
    }
  }

  val labels = new LoanLabels

  val referencedModels: Map[String,Model[_ <: Entity]] = Map(
    "userId" -> Users,
    "bookId" -> Books
  )

  def referencedModelsAndIds(entities: Seq[Loan])(implicit db: JdbcBackend#Database): Future[Map[Model[_ <: Entity],Map[Int,Option[(Int,String)]]]] = {
    val userEntities = Users.query.filter(
      _.id inSet entities.map(_.userId).distinct
    ).map(u => u.id -> (u.id -> u.tinyDescription))
    val bookEntities = Books.query.filter(
      _.id inSet entities.map(_.bookId).distinct
    ).map(b => b.id -> (b.id -> b.tinyDescription))
    for {
      users <- db.run(userEntities.result).map(_.toMap)
      books <- db.run(bookEntities.result).map(_.toMap)
    } yield Map(
      Users -> entities.map(e => e.id.get -> Some((e.userId, users.get(e.userId).map(_._2).getOrElse("")))).toMap,
      Books -> entities.map(e => e.id.get -> Some((e.bookId, books.get(e.bookId).map(_._2).getOrElse("")))).toMap
    )
  }

  override def tinyDescription(e: Loan) = e.name

  val schema = Map(
    "userId" -> ("Int", false),
    "bookId" -> ("Int", false),
    "loanDate" -> ("LocalDateTime", false),
    "dueDate" -> ("LocalDateTime", false),
    "returnDate" -> ("LocalDateTime", true),
    "status" -> ("String", false)
  )

  final val query = TableQuery[Loans]

  override val html = new HtmlForm

  class HtmlForm extends Html{
    def headings = Seq(labels.columns.id)
    def cells(e: Loan) = {
      def render(v: Any) = v match {
        case None => <em> - </em>
        case d:java.sql.Date => new java.text.SimpleDateFormat("dd MMM yyyy").format(d)
        case d:java.time.LocalDateTime => d.toString
        case v => v.toString
      }
      Seq[Any](e.id.getOrElse(0)).map{
        case Some(v) => render(v)
        case v => render(v)
      }
    }
  }
}

object Loans extends LoanModel

case class LoanForm(playForm: Form[Loan]) extends ModelForm[Loan]{
  val model = Loans
  override val html = new HtmlForm
  class HtmlForm extends Html{
    // ArrayBuffer()
    def allInputs(implicit handler: FieldConstructor, lang: Lang) = Seq(
      inputs.userId,
      inputs.bookId,
      inputs.loanDate,
      inputs.dueDate,
      inputs.returnDate,
      inputs.status
    )
    object inputs{
      def userId(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("userId"), Symbol("_label") -> model.labels.columns.userId)
      def bookId(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("bookId"), Symbol("_label") -> model.labels.columns.bookId)
      def loanDate(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("loanDate"), Symbol("_label") -> model.labels.columns.loanDate)
      def dueDate(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("dueDate"), Symbol("_label") -> model.labels.columns.dueDate)
      def returnDate(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("returnDate"), Symbol("_label") -> model.labels.columns.returnDate)
      def status(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("status"), Symbol("_label") -> model.labels.columns.status)
      def id(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("id"), Symbol("_label") -> model.labels.columns.id)
    }
  }
}

object loans extends TableQuery(tag => new Loans(tag))