package models

import scala.slick.driver.H2Driver.simple._
// import scala.slick.driver.PostgresDriver.simple._
import play.api.db.slick._
import play.api.db.slick.Session
import java.util.Date
import java.sql.{ Date => SqlDate }
// import play.api._
// import play.api.Play.current
// import play.api.libs.json._
import scala.slick.lifted.Tag
import views.html.helper._
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Lang

case class Loan(id: Option[Int] = None, email: String, password: String, name: String, permission: String) extends Entity

/** Table description of table COMPANY. Objects of this class serve as prototypes for rows in queries. */
abstract class LoansTable(tag: Tag) extends Table[Loan](tag, "Loan") with TableBase[Loan] {
  def * = (name, email, password, permission, id.?) <> (Loan.tupled, Loan.unapply)
  /** Maps whole row to an option. Useful for outer joins. */
  def ? = (name.?, email.?, password.?, permission.?, id.?).shaped.<>({r=>import r._; _1.map(_=> Loan.tupled((_5, _1.get, _2.get, _3.get, _4.get)))}, (_:Any) =>
    throw new Exception("Inserting into ? projection not supported."))

  /** Database column NAME  */
  val name: Column[String] = column[String]("NAME")
  val email: Column[String] = column[String]("EMAIL")
  val password: Column[String] = column[String]("PASSWORD")
  val permission: Column[String] = column[String]("PERMISSION")

  /** Database column ID AutoInc, PrimaryKey */
  val id: Column[Int] = column[Int]("ID", O.AutoInc, O.PrimaryKey)

  def tinyDescription = name

}
class Loans(tag: Tag) extends LoansTable(tag)

class LoanModel extends Model[Loan, Loans]{
  val playForm = Form(
    mapping(
      "id" -> optional(number),
      "name" -> nonEmptyText,
      "email" -> nonEmptyText,
      "password" -> nonEmptyText,
      "permission" -> nonEmptyText
    )(Loan.apply)(Loan.unapply)
  )
  def form(playForm: Form[Loan]) = LoanForm(playForm=playForm)

  class LoanLabels extends super.Labels {
    def singular = "Loan".toLowerCase
    def plural   = "Loans".toLowerCase
    object columns {
      def id: String = "Id"
      def name: String = "Name"
      def email: String = "Email"
      def password: String = "Password"
      def permission: String = "Permission"
    }
  }

  val labels = new LoanLabels

  val referencedModels: Map[String,Model[_ <: Entity,_]] = Map(

  )

  def referencedModelsAndIds(entities: Seq[Loan])(implicit session: Session): Map[Model[_ <: Entity,_],Map[Int,Option[(Int,String)]]] = {
    Map(

    )
  }

  override def tinyDescription(e: Loan) = e.name

  val schema = Map(
    "name" -> ("String", false),
    "email" -> ("String", false),
    "password" -> ("String", false),
    "permission" -> ("String", false)
  )

  final val query = TableQuery[Loans]

  override val html = new Html

  class Html extends super.Html{
    def headings = Seq(labels.columns.name)
    def cells(e: Loan) = {
      def render(v: Any) = v match {
        case None => <em> - </em>
        case d:java.sql.Date => new java.text.SimpleDateFormat("dd MMM yyyy").format(d)
        case v => v.toString
      }
      Seq[Any](e.name).map{
        case Some(v) => render(v)
        case v => render(v)
      }
    }
  }
}

object Loans extends LoanModel

case class LoanForm(playForm: Form[Loan]) extends ModelForm[Loan]{
  val model = Loans
  override val html = new Html
  class Html extends super.Html{
    // ArrayBuffer()
    def allInputs(implicit handler: FieldConstructor, lang: Lang) = Seq(
      inputs.name
    )
    object inputs{
      def name(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("name"), '_label -> model.labels.columns.name)
      def email(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("email"), '_label -> model.labels.columns.email)
      def password(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("password"), '_label -> model.labels.columns.password)
      def permission(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("permission"), '_label -> model.labels.columns.permission)
      def id(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("id"), '_label -> model.labels.columns.id)
    }
  }
}

object loans extends TableQuery(tag => new Loans(tag))