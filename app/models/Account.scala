package models

import scala.slick.driver.H2Driver.simple._
// import scala.slick.driver.PostgresDriver.simple._
import play.api.db.DB
import play.api.db.slick._
import java.util.Date
import java.sql.{ Date => SqlDate }
import play.api._
import play.api.Play.current
import play.api.libs.json._
import scala.slick.lifted.Tag
import views.html.helper._
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Lang

case class Account(id: Option[Int] = None, email: String, password: String, name: String, permission: String) extends Entity
/** Table description of table COMPANY. Objects of this class serve as prototypes for rows in queries. */
abstract class AccountsTable(tag: Tag) extends Table[Account](tag, "ACCOUNT") with TableBase[Account] {
  def * = (name, email, password, permission, id.?) <> (Account.tupled, Account.unapply)
  /** Maps whole row to an option. Useful for outer joins. */
  def ? = (name.?, email.?, password.?, permission.?, id.?).shaped.<>({r=>import r._; _1.map(_=> Account.tupled((_5, _1.get, _2.get, _3.get, _4.get)))}, (_:Any) =>
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
class Accounts(tag: Tag) extends AccountsTable(tag)

class AccountModel extends Model[Account, Accounts]{
  val playForm = Form(
    mapping(
      "id" -> optional(number),
      "name" -> nonEmptyText,
      "email" -> nonEmptyText,
      "password" -> nonEmptyText,
      "permission" -> nonEmptyText
    )(Account.apply)(Account.unapply)
  )
  def form(playForm: Form[Account]) = AccountForm(playForm=playForm)

  val labels = new super.Labels{
    def singular = "Account".toLowerCase
    def plural   = "Accounts".toLowerCase
    object columns {
      def id: String = "Id"
      def name: String = "Name"
      def email: String = "Email"
      def password: String = "Password"
      def permission: String = "Permission"
    }
  }

  val referencedModels: Map[String,Model[_ <: Entity,_]] = Map(

  )

  def referencedModelsAndIds(entities: Seq[Account])(implicit session: Session): Map[Model[_ <: Entity,_],Map[Int,Option[(Int,String)]]] = {
    Map(

    )
  }

  override def tinyDescription(e: Account) = e.name

  val schema = Map(
    "name" -> ("String", false),
    "email" -> ("String", false),
    "password" -> ("String", false),
    "permission" -> ("String", false)
  )

  final val query = TableQuery[Accounts]

  override val html = new Html

  class Html extends super.Html{
    def headings = Seq(labels..columns.name)
    def cells(e: Account) = {
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

object Accounts extends AccountModel
case class AccountForm(playForm: Form[Account]) extends ModelForm[Account]{
  val model = Accounts
  override val html = new Html
  class Html extends super.Html{
    // ArrayBuffer()
    def allInputs(implicit handler: FieldConstructor, lang: Lang) = Seq(
      inputs.name
    )
    object inputs{
      def name(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("name"), '_label -> model.labels.columns.name)
      def id(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("id"), '_label -> model.labels.columns.id)
    }
  }
}

