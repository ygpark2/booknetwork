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

case class Book(id: Option[Int] = None, title: String, author: String, description: Option[String], ownerId: Int, genre: Option[String], isbn: Option[String], available: Boolean = true) extends Entity

/** Table description of table COMPANY. Objects of this class serve as prototypes for rows in queries. */
abstract class BooksTable(tag: Tag) extends Table[Book](tag, "BOOK") with TableBase[Book] {
  def * = (id.?, title, author, description, ownerId, genre, isbn, available) <> (Book.tupled, Book.unapply)
  /** Maps whole row to an option. Useful for outer joins. */
  def ? = (id.?, title.?, author.?, description, ownerId.?, genre, isbn, available.?).shaped.<>({r=>import r._; _1.map(_=> Book.tupled((_1, _2.get, _3.get, _4, _5.get, _6, _7, _8.getOrElse(true))))}, (_:Any) =>
    throw new Exception("Inserting into ? projection not supported."))

  /** Database columns */
  val title: Rep[String] = column[String]("TITLE")
  val author: Rep[String] = column[String]("AUTHOR")
  val description: Rep[Option[String]] = column[Option[String]]("DESCRIPTION")
  val ownerId: Rep[Int] = column[Int]("OWNER_ID")
  val genre: Rep[Option[String]] = column[Option[String]]("GENRE")
  val isbn: Rep[Option[String]] = column[Option[String]]("ISBN")
  val available: Rep[Boolean] = column[Boolean]("AVAILABLE", O.Default(true))

  /** Database column ID AutoInc, PrimaryKey */
  val id: Column[Int] = column[Int]("ID", O.AutoInc, O.PrimaryKey)

  def tinyDescription = title

}
class Books(tag: Tag) extends BooksTable(tag)

class BookModel extends Model[Book]{
  val playForm = Form(
    mapping(
      "id" -> optional(number),
      "title" -> nonEmptyText,
      "author" -> nonEmptyText,
      "description" -> optional(text),
      "ownerId" -> number,
      "genre" -> optional(text),
      "isbn" -> optional(text),
      "available" -> boolean
    )(Book.apply)(Book.unapply)
  )
  def form(playForm: Form[Book]) = BookForm(playForm=playForm)

  class BookLabels extends super.Labels {
    def singular = "Book".toLowerCase
    def plural   = "Books".toLowerCase
    object columns {
      def id: String = "Id"
      def title: String = "Title"
      def author: String = "Author"
      def description: String = "Description"
      def ownerId: String = "Owner ID"
      def genre: String = "Genre"
      def isbn: String = "ISBN"
      def available: String = "Available"
    }
  }

  val labels = new BookLabels

  val referencedModels: Map[String,Model[_ <: Entity]] = Map(
    "ownerId" -> Users
  )

  def referencedModelsAndIds(entities: Seq[Book])(implicit db: JdbcBackend#Database): Future[Map[Model[_ <: Entity],Map[Int,Option[(Int,String)]]]] = {
    Future.successful(Map(
      Users -> entities.map(e => e.id.get -> Some((e.ownerId, ""))).toMap
    ))
  }

  override def tinyDescription(e: Book) = e.name

  val schema = Map(
    "title" -> ("String", false),
    "author" -> ("String", false),
    "description" -> ("String", true),
    "ownerId" -> ("Int", false),
    "genre" -> ("String", true),
    "isbn" -> ("String", true),
    "available" -> ("Boolean", false)
  )

  final val query = TableQuery[Books]

  override val html = new HtmlForm

  class HtmlForm extends Html{
    def headings = Seq(labels.columns.title)
    def cells(e: Book) = {
      def render(v: Any) = v match {
        case None => <em> - </em>
        case d:java.sql.Date => new java.text.SimpleDateFormat("dd MMM yyyy").format(d)
        case v => v.toString
      }
      Seq[Any](e.title).map{
        case Some(v) => render(v)
        case v => render(v)
      }
    }
  }
}

object Books extends BookModel

case class BookForm(playForm: Form[Book]) extends ModelForm[Book]{
  val model = Books
  override val html = new HtmlForm
  class HtmlForm extends Html{
    // ArrayBuffer()
    def allInputs(implicit handler: FieldConstructor, lang: Lang) = Seq(
      inputs.title,
      inputs.author,
      inputs.description,
      inputs.ownerId,
      inputs.genre,
      inputs.isbn,
      inputs.available
    )
    object inputs{
      def title(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("title"), Symbol("_label") -> model.labels.columns.title)
      def author(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("author"), Symbol("_label") -> model.labels.columns.author)
      def description(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("description"), Symbol("_label") -> model.labels.columns.description)
      def ownerId(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("ownerId"), Symbol("_label") -> model.labels.columns.ownerId)
      def genre(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("genre"), Symbol("_label") -> model.labels.columns.genre)
      def isbn(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("isbn"), Symbol("_label") -> model.labels.columns.isbn)
      def available(implicit handler: FieldConstructor, lang: Lang) = inputCheckbox(playForm("available"), Symbol("_label") -> model.labels.columns.available)
      def id(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("id"), Symbol("_label") -> model.labels.columns.id)
    }
  }
}

object books extends TableQuery(tag => new Books(tag))