package models

import slick.jdbc.H2Profile.api._
import slick.jdbc.JdbcBackend
import javax.inject.Inject
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import views.html.helper._
import play.api.data.Form
import play.api.data.Forms._

case class User(id: Option[Int] = None, email: String, password: String, name: String, bio: Option[String], avatar: Option[String], location: Option[String], createdAt: java.time.LocalDateTime) extends Entity

class Users(tag: Tag) extends Table[User](tag, "USER") with TableBase[User] {
  def * = (id.?, email, password, name, bio, avatar, location, createdAt).mapTo[User]

  val id: Rep[Int] = column[Int]("ID", O.PrimaryKey, O.AutoInc)
  val email: Rep[String] = column[String]("EMAIL")
  val password: Rep[String] = column[String]("PASSWORD")
  val name: Rep[String] = column[String]("NAME")
  val bio: Rep[Option[String]] = column[Option[String]]("BIO")
  val avatar: Rep[Option[String]] = column[Option[String]]("AVATAR")
  val location: Rep[Option[String]] = column[Option[String]]("LOCATION")
  val createdAt: Rep[java.time.LocalDateTime] = column[java.time.LocalDateTime]("CREATED_AT", O.Default(java.time.LocalDateTime.now()))

  def tinyDescription = name
}

class UserModel(implicit db: JdbcBackend#Database) extends Model[User]{
  val playForm = Form(
    mapping(
      "id" -> optional(number),
      "email" -> nonEmptyText,
      "password" -> nonEmptyText,
      "name" -> nonEmptyText,
      "bio" -> optional(text),
      "avatar" -> optional(text),
      "location" -> optional(text),
      "createdAt" -> localDateTime
    )(User.apply)(User.unapply)
  )
  def form(playForm: Form[User]) = UserForm(playForm=playForm)

  class UserLabels extends super.Labels {
    def singular = "User".toLowerCase
    def plural   = "Users".toLowerCase
    object columns {
      def id: String = "Id"
      def email: String = "Email"
      def password: String = "Password"
      def name: String = "Name"
      def bio: String = "Bio"
      def avatar: String = "Avatar"
      def location: String = "Location"
      def createdAt: String = "Created At"
    }
  }

  val labels = new UserLabels

  val referencedModels: Map[String,Model[_ <: Entity]] = Map(

  )

  def referencedModelsAndIds(entities: Seq[User])(implicit db: JdbcBackend#Database): Future[Map[Model[_ <: Entity],Map[Int,Option[(Int,String)]]]] = {
    Future.successful(Map.empty)
  }

  override def tinyDescription(e: User) = e.name

  val schema = Map(
    "email" -> ("String", false),
    "password" -> ("String", false),
    "name" -> ("String", false),
    "bio" -> ("String", true),
    "avatar" -> ("String", true),
    "location" -> ("String", true),
    "createdAt" -> ("LocalDateTime", false)
  )

  final val query = TableQuery[Users]

  override val html = new HtmlForm

  class HtmlForm extends Html{
    def headings = Seq(labels.columns.name)
    def cells(e: User) = {
      def render(v: Any) = v match {
        case None => <em> - </em>
        case d:java.time.LocalDate => d.toString
        case d:java.time.LocalDateTime => d.toString
        case v => v.toString
      }
      Seq[Any](e.name).map{
        case Some(v) => render(v)
        case v => render(v)
      }
    }
  }
}

object Users extends UserModel

case class UserForm(playForm: Form[User]) extends ModelForm[User]{
  val model = Users
  override val html = new HtmlForm
  class HtmlForm extends Html{
    def allInputs(implicit handler: FieldConstructor) = Seq(
      inputs.email,
      inputs.password,
      inputs.name,
      inputs.bio,
      inputs.avatar,
      inputs.location
    )
    object inputs{
      def email(implicit handler: FieldConstructor) = inputText(playForm("email"), Symbol("_label") -> model.labels.columns.email)
      def password(implicit handler: FieldConstructor) = inputPassword(playForm("password"), Symbol("_label") -> model.labels.columns.password)
      def name(implicit handler: FieldConstructor) = inputText(playForm("name"), Symbol("_label") -> model.labels.columns.name)
      def bio(implicit handler: FieldConstructor) = inputText(playForm("bio"), Symbol("_label") -> model.labels.columns.bio)
      def avatar(implicit handler: FieldConstructor) = inputText(playForm("avatar"), Symbol("_label") -> model.labels.columns.avatar)
      def location(implicit handler: FieldConstructor) = inputText(playForm("location"), Symbol("_label") -> model.labels.columns.location)
      def id(implicit handler: FieldConstructor) = inputText(playForm("id"), Symbol("_label") -> model.labels.columns.id)
    }
  }
}
