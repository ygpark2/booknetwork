package controllers

import play.api.data._
import play.api.data.Forms._
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesAbstractController, MessagesControllerComponents}
import repositories.{BookRepository, UserRepository}

import forms.BookForm

import javax.inject._
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class BookController @Inject()(cc: MessagesControllerComponents, bookRepository: BookRepository, userRepository: UserRepository)(implicit ec: ExecutionContext)
  extends MessagesAbstractController(cc) with I18nSupport {

  val bookForm: Form[BookForm] = Form(
    mapping(
      "title" -> nonEmptyText,
      "author" -> nonEmptyText,
      "description" -> optional(text),
      "metadata" -> mapping(
        "subtitle" -> optional(text),
        "isbn10" -> optional(text),
        "isbn13" -> optional(text),
        "issn" -> optional(text),
        "publisher" -> optional(text),
        "publicationYear" -> optional(number),
        "publicationPlace" -> optional(text),
        "edition" -> optional(text),
        "seriesTitle" -> optional(text),
        "seriesNumber" -> optional(text),
        "language" -> optional(text),
        "originalLanguage" -> optional(text),
        "subjects" -> optional(text),
        "keywords" -> optional(text),
        "summary" -> optional(text),
        "classification" -> optional(text),
        "callNumberBase" -> optional(text),
        "format" -> optional(text),
        "pages" -> optional(number),
        "dimensions" -> optional(text),
        "coverImageUrl" -> optional(text),
        "thumbnailUrl" -> optional(text)
      )(
        (
          subtitle,
          isbn10,
          isbn13,
          issn,
          publisher,
          publicationYear,
          publicationPlace,
          edition,
          seriesTitle,
          seriesNumber,
          language,
          originalLanguage,
          subjects,
          keywords,
          summary,
          classification,
          callNumberBase,
          format,
          pages,
          dimensions,
          coverImageUrl,
          thumbnailUrl
        ) => models.BookMetadata(
          subtitle = subtitle,
          isbn10 = isbn10,
          isbn13 = isbn13,
          issn = issn,
          publisher = publisher,
          publicationYear = publicationYear,
          publicationPlace = publicationPlace,
          edition = edition,
          seriesTitle = seriesTitle,
          seriesNumber = seriesNumber,
          language = language,
          originalLanguage = originalLanguage,
          subjects = subjects,
          keywords = keywords,
          summary = summary,
          classification = classification,
          callNumberBase = callNumberBase,
          format = format,
          pages = pages,
          dimensions = dimensions,
          coverImageUrl = coverImageUrl,
          thumbnailUrl = thumbnailUrl
        )
      )(
        metadata => Some(
          (
            metadata.subtitle,
            metadata.isbn10,
            metadata.isbn13,
            metadata.issn,
            metadata.publisher,
            metadata.publicationYear,
            metadata.publicationPlace,
            metadata.edition,
            metadata.seriesTitle,
            metadata.seriesNumber,
            metadata.language,
            metadata.originalLanguage,
            metadata.subjects,
            metadata.keywords,
            metadata.summary,
            metadata.classification,
            metadata.callNumberBase,
            metadata.format,
            metadata.pages,
            metadata.dimensions,
            metadata.coverImageUrl,
            metadata.thumbnailUrl
          )
        )
      )
    )(BookForm.apply)(BookForm.unapply)
  )


  def list: Action[AnyContent] = Action.async { implicit request =>
    bookRepository.list().map { books =>
      implicit val messages = request.messages
      Ok(views.html.books.list(books))
    }
  }

  private def requireAuth[A](block: => Future[play.api.mvc.Result])(implicit request: play.api.mvc.Request[A]): Future[play.api.mvc.Result] = {
    request.session.get("userEmail") match {
      case Some(_) => block
      case None => Future.successful(Unauthorized("You must be authenticated to access this page."))
    }
  }

  def create: Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      Future.successful(Ok(views.html.books.create(bookForm)))
    }
  }

  private val itemForm = Form(
    mapping(
      "barcode" -> nonEmptyText,
      "callNumber" -> optional(text),
      "location" -> optional(text)
    )((barcode, callNumber, location) => (barcode, callNumber, location))
      ({ form => Some((form._1, form._2, form._3)) })
  )

  def detail(id: Long): Action[AnyContent] = Action.async { implicit request =>
    bookRepository.findById(id).flatMap {
      case Some(book) =>
        bookRepository.findItemsByBookId(id).map { items =>
          Ok(views.html.books.detail(book, items, itemForm))
        }
      case None =>
        Future.successful(NotFound("Book not found"))
    }
  }

  def edit(id: Long): Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      bookRepository.findById(id).flatMap {
        case Some(book) =>
          val filledForm = bookForm.fill(
            BookForm(
              title = book.title,
              author = book.author,
              description = book.description,
              metadata = book.metadata
            )
          )
          Future.successful(Ok(views.html.books.edit(id, filledForm)))
        case None =>
          Future.successful(NotFound("Book not found"))
      }
    }
  }

  def update(id: Long): Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      bookForm.bindFromRequest().fold(
        formWithErrors => Future.successful(BadRequest(views.html.books.edit(id, formWithErrors))),
        data => {
          request.session.get("userEmail") match {
            case Some(email) =>
              userRepository.findByEmail(email).flatMap {
                case Some(user) =>
                  val updatedBook = models.Book(
                    id = id,
                    ownerId = user.id,
                    title = data.title,
                    author = data.author,
                    description = data.description,
                    metadata = data.metadata
                  )
                  bookRepository.update(id, updatedBook).map { _ =>
                    Redirect(routes.BookController.detail(id)).flashing("success" -> "Book updated")
                  }
                case None =>
                  Future.successful(Unauthorized("User not found."))
              }
            case None =>
              Future.successful(Unauthorized("You must be authenticated to access this page."))
          }
        }
      )
    }
  }

  def addItem(id: Long): Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      itemForm.bindFromRequest().fold(
        formWithErrors =>
          bookRepository.findById(id).flatMap {
            case Some(book) =>
              bookRepository.findItemsByBookId(id).map { items =>
                BadRequest(views.html.books.detail(book, items, formWithErrors))
              }
            case None =>
              Future.successful(NotFound("Book not found"))
          },
        data =>
          bookRepository.addItem(id, data._1, data._2, data._3).map { _ =>
            Redirect(routes.BookController.detail(id)).flashing("success" -> "Item added")
          }
      )
    }
  }

  def save: Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      bookForm.bindFromRequest().fold(
        formWithErrors => Future.successful(BadRequest(views.html.books.create(formWithErrors))),
        data => {
          request.session.get("userEmail") match {
            case Some(email) =>
              userRepository.findByEmail(email).flatMap {
                case Some(user) =>
                  val book = models.Book(
                    ownerId = user.id,
                    title = data.title,
                    author = data.author,
                    description = data.description,
                    metadata = data.metadata
                  )
                  val item = models.BookItem(
                    bookId = 0L,
                    barcode = java.util.UUID.randomUUID().toString
                  )
                  bookRepository.insertWithItem(book, item).map { _ =>
                    Redirect(routes.BookController.list).flashing("success" -> "Book added")
                  }
                case None =>
                  Future.successful(Unauthorized("User not found."))
              }
            case None =>
              Future.successful(Unauthorized("You must be authenticated to access this page."))
          }
        }
      )
    }
  }
}
