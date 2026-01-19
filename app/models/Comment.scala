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

case class Comment(id: Option[Int] = None, userId: Int, postId: Int, content: String, createdAt: java.time.LocalDateTime) extends Entity

abstract class CommentsTable(tag: Tag) extends Table[Comment](tag, "COMMENT") with TableBase[Comment] {
  def * = (id.?, userId, postId, content, createdAt) <> (Comment.tupled, Comment.unapply)
  def ? = (id.?, userId.?, postId.?, content.?, createdAt.?).shaped.<>({r=>import r._; _1.map(_=> Comment.tupled((_1, _2.get, _3.get, _4.get, _5.get)))}, (_:Any) =>
    throw new Exception("Inserting into ? projection not supported."))

  val userId: Rep[Int] = column[Int]("USER_ID")
  val postId: Rep[Int] = column[Int]("POST_ID")
  val content: Rep[String] = column[String]("CONTENT")
  val createdAt: Rep[java.time.LocalDateTime] = column[java.time.LocalDateTime]("CREATED_AT", O.Default(java.time.LocalDateTime.now()))

  val id: Column[Int] = column[Int]("ID", O.AutoInc, O.PrimaryKey)

  def tinyDescription = content.take(50) + "..."

}

class Comments(tag: Tag) extends CommentsTable(tag)

class CommentModel extends Model[Comment]{
  val playForm = Form(
    mapping(
      "id" -> optional(number),
      "userId" -> number,
      "postId" -> number,
      "content" -> nonEmptyText,
      "createdAt" -> localDateTime
    )(Comment.apply)(Comment.unapply)
  )
  def form(playForm: Form[Comment]) = CommentForm(playForm=playForm)

  class CommentLabels extends super.Labels {
    def singular = "Comment".toLowerCase
    def plural   = "Comments".toLowerCase
    object columns {
      def id: String = "Id"
      def userId: String = "User ID"
      def postId: String = "Post ID"
      def content: String = "Content"
      def createdAt: String = "Created At"
    }
  }

  val labels = new CommentLabels

  val referencedModels: Map[String,Model[_ <: Entity]] = Map(
    "userId" -> Users,
    "postId" -> Posts
  )

  def referencedModelsAndIds(entities: Seq[Comment])(implicit db: JdbcBackend#Database): Future[Map[Model[_ <: Entity],Map[Int,Option[(Int,String)]]]] = {
    Future.successful(Map(
      Users -> entities.map(e => e.id.get -> Some((e.userId, ""))).toMap,
      Posts -> entities.map(e => e.id.get -> Some((e.postId, ""))).toMap
    ))
  }

  override def tinyDescription(e: Comment) = e.content.take(50) + "..."

  val schema = Map(
    "userId" -> ("Int", false),
    "postId" -> ("Int", false),
    "content" -> ("String", false),
    "createdAt" -> ("LocalDateTime", false)
  )

  final val query = TableQuery[Comments]

  override val html = new HtmlForm

  class HtmlForm extends Html{
    def headings = Seq(labels.columns.content)
    def cells(e: Comment) = {
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

object Comments extends CommentModel

case class CommentForm(playForm: Form[Comment]) extends ModelForm[Comment]{
  val model = Comments
  override val html = new HtmlForm
  class HtmlForm extends Html{
    def allInputs(implicit handler: FieldConstructor, lang: Lang) = Seq(
      inputs.userId,
      inputs.postId,
      inputs.content
    )
    object inputs{
      def userId(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("userId"), Symbol("_label") -> model.labels.columns.userId)
      def postId(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("postId"), Symbol("_label") -> model.labels.columns.postId)
      def content(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("content"), Symbol("_label") -> model.labels.columns.content)
      def id(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("id"), Symbol("_label") -> model.labels.columns.id)
    }
  }
}

object comments extends TableQuery(tag => new Comments(tag))