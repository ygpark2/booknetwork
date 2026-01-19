package models

import slick.jdbc.H2Profile.api._
import slick.jdbc.JdbcBackend
import javax.inject.Inject
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import views.html.helper._
import play.api.data.Form
import play.api.data.Forms._

case class Account(id: Option[Int] = None, email: String, password: String, name: String, permission: String) extends Entity

class Accounts(tag: Tag) extends Table[Account](tag, "ACCOUNT") with TableBase[Account] {
  def * = (id.?, email, password, name, permission).mapTo[Account]

  val id: Rep[Int] = column[Int]("ID", O.PrimaryKey, O.AutoInc)
  val email: Rep[String] = column[String]("EMAIL")
  val password: Rep[String] = column[String]("PASSWORD")
  val name: Rep[String] = column[String]("NAME")
  val permission: Rep[String] = column[String]("PERMISSION")

  def tinyDescription = name
}

class AccountModel(implicit db: JdbcBackend#Database) extends Model[Account]{
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

  class AccountLabels extends super.Labels {
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

  val labels = new AccountLabels

  val referencedModels: Map[String,Model[_ <: Entity,_]] = Map(

  )

  def referencedModelsAndIds(entities: Seq[Account])(implicit db: JdbcBackend#Database): Future[Map[Model[_ <: Entity],Map[Int,Option[(Int,String)]]]] = {
    Future.successful(Map.empty)
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
    def headings = Seq(labels.columns.name)
    def cells(e: Account) = {
      def render(v: Any) = v match {
        case None => <em> - </em>
        case d:java.time.LocalDate => d.toString
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
    def allInputs(implicit handler: FieldConstructor) = Seq(
      inputs.name
    )
    object inputs{
      def name(implicit handler: FieldConstructor) = inputText(playForm("name"), '_label -> model.labels.columns.name)
      def email(implicit handler: FieldConstructor) = inputText(playForm("email"), '_label -> model.labels.columns.email)
      def password(implicit handler: FieldConstructor) = inputText(playForm("password"), '_label -> model.labels.columns.password)
      def permission(implicit handler: FieldConstructor) = inputText(playForm("permission"), '_label -> model.labels.columns.permission)
      def id(implicit handler: FieldConstructor) = inputText(playForm("id"), '_label -> model.labels.columns.id)
    }
  }
}
