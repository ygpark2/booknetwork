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

case class Review(id: Option[Int] = None, userId: Int, bookId: Int, rating: Int, comment: Option[String], createdAt: java.time.LocalDateTime) extends Entity

abstract class ReviewsTable(tag: Tag) extends Table[Review](tag, "REVIEW") with TableBase[Review] {
  def * = (id.?, userId, bookId, rating, comment, createdAt) <> (Review.tupled, Review.unapply)
  def ? = (id.?, userId.?, bookId.?, rating.?, comment, createdAt.?).shaped.<>({r=>import r._; _1.map(_=> Review.tupled((_1, _2.get, _3.get, _4.get, _5, _6.get)))}, (_:Any) =>
    throw new Exception("Inserting into ? projection not supported."))

  val userId: Rep[Int] = column[Int]("USER_ID")
  val bookId: Rep[Int] = column[Int]("BOOK_ID")
  val rating: Rep[Int] = column[Int]("RATING")
  val comment: Rep[Option[String]] = column[Option[String]]("COMMENT")
  val createdAt: Rep[java.time.LocalDateTime] = column[java.time.LocalDateTime]("CREATED_AT", O.Default(java.time.LocalDateTime.now()))

  val id: Column[Int] = column[Int]("ID", O.AutoInc, O.PrimaryKey)

  def tinyDescription = s"Review: ${rating} stars"

}

class Reviews(tag: Tag) extends ReviewsTable(tag)

class ReviewModel extends Model[Review]{
  val playForm = Form(
    mapping(
      "id" -> optional(number),
      "userId" -> number,
      "bookId" -> number,
      "rating" -> number(min=1, max=5),
      "comment" -> optional(text),
      "createdAt" -> localDateTime
    )(Review.apply)(Review.unapply)
  )
  def form(playForm: Form[Review]) = ReviewForm(playForm=playForm)

  class ReviewLabels extends super.Labels {
    def singular = "Review".toLowerCase
    def plural   = "Reviews".toLowerCase
    object columns {
      def id: String = "Id"
      def userId: String = "User ID"
      def bookId: String = "Book ID"
      def rating: String = "Rating"
      def comment: String = "Comment"
      def createdAt: String = "Created At"
    }
  }

  val labels = new ReviewLabels

  val referencedModels: Map[String,Model[_ <: Entity]] = Map(
    "userId" -> Users,
    "bookId" -> Books
  )

  def referencedModelsAndIds(entities: Seq[Review])(implicit db: JdbcBackend#Database): Future[Map[Model[_ <: Entity],Map[Int,Option[(Int,String)]]]] = {
    Future.successful(Map(
      Users -> entities.map(e => e.id.get -> Some((e.userId, ""))).toMap,
      Books -> entities.map(e => e.id.get -> Some((e.bookId, ""))).toMap
    ))
  }

  override def tinyDescription(e: Review) = s"Review: ${e.rating} stars"

  val schema = Map(
    "userId" -> ("Int", false),
    "bookId" -> ("Int", false),
    "rating" -> ("Int", false),
    "comment" -> ("String", true),
    "createdAt" -> ("LocalDateTime", false)
  )

  final val query = TableQuery[Reviews]

  override val html = new HtmlForm

  class HtmlForm extends Html{
    def headings = Seq(labels.columns.rating, labels.columns.comment)
    def cells(e: Review) = {
      def render(v: Any) = v match {
        case None => <em> - </em>
        case d:java.sql.Date => new java.text.SimpleDateFormat("dd MMM yyyy").format(d)
        case d:java.time.LocalDateTime => d.toString
        case v => v.toString
      }
      Seq[Any](e.rating, e.comment.getOrElse("-")).map{
        case Some(v) => render(v)
        case v => render(v)
      }
    }
  }
}

object Reviews extends ReviewModel

case class ReviewForm(playForm: Form[Review]) extends ModelForm[Review]{
  val model = Reviews
  override val html = new HtmlForm
  class HtmlForm extends Html{
    def allInputs(implicit handler: FieldConstructor, lang: Lang) = Seq(
      inputs.userId,
      inputs.bookId,
      inputs.rating,
      inputs.comment
    )
    object inputs{
      def userId(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("userId"), Symbol("_label") -> model.labels.columns.userId)
      def bookId(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("bookId"), Symbol("_label") -> model.labels.columns.bookId)
      def rating(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("rating"), Symbol("_label") -> model.labels.columns.rating)
      def comment(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("comment"), Symbol("_label") -> model.labels.columns.comment)
      def id(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("id"), Symbol("_label") -> model.labels.columns.id)
    }
  }
}

object reviews extends TableQuery(tag => new Reviews(tag))