package models

import slick.jdbc.H2Profile.api._
import play.api.db.slick._
import java.util.Date
import java.sql.{ Date => SqlDate }
import slick.lifted.Tag
import views.html.helper._
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Lang

case class Post(id: Option[Int] = None, userId: Int, content: String, image: Option[String], createdAt: java.time.LocalDateTime) extends Entity

abstract class PostsTable(tag: Tag) extends Table[Post](tag, "POST") with TableBase[Post] {
  def * = (id.?, userId, content, image, createdAt) <> (Post.tupled, Post.unapply)
  def ? = (id.?, userId.?, content.?, image, createdAt.?).shaped.<>({r=>import r._; _1.map(_=> Post.tupled((_1, _2.get, _3.get, _4, _5.get)))}, (_:Any) =>
    throw new Exception("Inserting into ? projection not supported."))

  val userId: Rep[Int] = column[Int]("USER_ID")
  val content: Rep[String] = column[String]("CONTENT")
  val image: Rep[Option[String]] = column[Option[String]]("IMAGE")
  val createdAt: Rep[java.time.LocalDateTime] = column[java.time.LocalDateTime]("CREATED_AT", O.Default(java.time.LocalDateTime.now()))

  val id: Column[Int] = column[Int]("ID", O.AutoInc, O.PrimaryKey)

  def tinyDescription = content.take(50) + "..."

}

class Posts(tag: Tag) extends PostsTable(tag)

class PostModel extends Model[Post]{
  val playForm = Form(
    mapping(
      "id" -> optional(number),
      "userId" -> number,
      "content" -> nonEmptyText,
      "image" -> optional(text),
      "createdAt" -> localDateTime
    )(Post.apply)(Post.unapply)
  )
  def form(playForm: Form[Post]) = PostForm(playForm=playForm)

  class PostLabels extends super.Labels {
    def singular = "Post".toLowerCase
    def plural   = "Posts".toLowerCase
    object columns {
      def id: String = "Id"
      def userId: String = "User ID"
      def content: String = "Content"
      def image: String = "Image"
      def createdAt: String = "Created At"
    }
  }

  val labels = new PostLabels

  val referencedModels: Map[String,Model[_ <: Entity]] = Map(
    "userId" -> Users
  )

  def referencedModelsAndIds(entities: Seq[Post])(implicit db: JdbcBackend#Database): Future[Map[Model[_ <: Entity],Map[Int,Option[(Int,String)]]]] = {
    Future.successful(Map(
      Users -> entities.map(e => e.id.get -> Some((e.userId, ""))).toMap
    ))
  }

  override def tinyDescription(e: Post) = e.content.take(50) + "..."

  val schema = Map(
    "userId" -> ("Int", false),
    "content" -> ("String", false),
    "image" -> ("String", true),
    "createdAt" -> ("LocalDateTime", false)
  )

  final val query = TableQuery[Posts]

  override val html = new HtmlForm

  class HtmlForm extends Html{
    def headings = Seq(labels.columns.content)
    def cells(e: Post) = {
      def render(v: Any) = v match {
        case None => <em> - </em>
        case d:java.sql.Date => new java.text.SimpleDateFormat("dd MMM yyyy").format(d)
        case d:java.time.LocalDateTime => d.toString
        case v => v.toString
      }
      Seq[Any](e.content.take(50) + "...").map{
        case Some(v) => render(v)
        case v => render(v)
      }
    }
  }
}

object Posts extends PostModel

case class PostForm(playForm: Form[Post]) extends ModelForm[Post]{
  val model = Posts
  override val html = new HtmlForm
  class HtmlForm extends Html{
    def allInputs(implicit handler: FieldConstructor, lang: Lang) = Seq(
      inputs.userId,
      inputs.content,
      inputs.image
    )
    object inputs{
      def userId(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("userId"), Symbol("_label") -> model.labels.columns.userId)
      def content(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("content"), Symbol("_label") -> model.labels.columns.content)
      def image(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("image"), Symbol("_label") -> model.labels.columns.image)
      def id(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("id"), Symbol("_label") -> model.labels.columns.id)
    }
  }
}

object posts extends TableQuery(tag => new Posts(tag))