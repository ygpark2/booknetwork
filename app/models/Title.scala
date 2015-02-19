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

case class Title(id: Option[Int] = None, email: String, password: String, name: String, permission: String) extends Entity

/** Table description of table COMPANY. Objects of this class serve as prototypes for rows in queries. */
abstract class TitlesTable(tag: Tag) extends Table[Title](tag, "TITLE") with TableBase[Title] {
  def * = (id.?, email, password, name, permission) <> (Title.tupled, Title.unapply)
  /** Maps whole row to an option. Useful for outer joins. */
  def ? = (id.?, email.?, password.?, name.?, permission.?).shaped.<>({r=>import r._; _1.map(_=> Title.tupled((_1, _2.get, _3.get, _4.get, _5.get)))}, (_:Any) =>
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
class Titles(tag: Tag) extends TitlesTable(tag)

class TitleModel extends Model[Title, Titles]{
  val playForm = Form(
    mapping(
      "id" -> optional(number),
      "name" -> nonEmptyText,
      "email" -> nonEmptyText,
      "password" -> nonEmptyText,
      "permission" -> nonEmptyText
    )(Title.apply)(Title.unapply)
  )
  def form(playForm: Form[Title]) = TitleForm(playForm=playForm)

  class TitleLabels extends super.Labels {
    def singular = "Title".toLowerCase
    def plural   = "Titles".toLowerCase
    object columns {
      def id: String = "Id"
      def name: String = "Name"
      def email: String = "Email"
      def password: String = "Password"
      def permission: String = "Permission"
    }
  }

  val labels = new TitleLabels

  val referencedModels: Map[String,Model[_ <: Entity,_]] = Map(

  )

  def referencedModelsAndIds(entities: Seq[Title])(implicit session: Session): Map[Model[_ <: Entity,_],Map[Int,Option[(Int,String)]]] = {
    Map(

    )
  }

  override def tinyDescription(e: Title) = e.name

  val schema = Map(
    "name" -> ("String", false),
    "email" -> ("String", false),
    "password" -> ("String", false),
    "permission" -> ("String", false)
  )

  final val query = TableQuery[Titles]

  override val html = new Html

  class Html extends super.Html{
    def headings = Seq(labels.columns.name)
    def cells(e: Title) = {
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

object Titles extends TitleModel

case class TitleForm(playForm: Form[Title]) extends ModelForm[Title]{
  val model = Titles
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

object titles extends TableQuery(tag => new Titles(tag))