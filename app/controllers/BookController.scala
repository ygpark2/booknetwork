package controllers

import play.api.data._
import play.api.data.Forms._
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesAbstractController, MessagesControllerComponents}
import repositories.BookRepository

import forms.BookForm

import javax.inject._
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class BookController @Inject()(cc: MessagesControllerComponents, bookRepository: BookRepository)(implicit ec: ExecutionContext)
  extends MessagesAbstractController(cc) with I18nSupport {

  val bookForm: Form[BookForm] = Form(
    mapping(
      "title" -> nonEmptyText,
      "author" -> nonEmptyText,
      "description" -> optional(text)
    )((title, author, description) => BookForm(title, author, description))({ form =>
      Some((form.title, form.author, form.description))
    })
  )


  def list: Action[AnyContent] = Action.async { implicit request =>
    bookRepository.list().map { books =>
      implicit val messages = request.messages
      Ok(views.html.books.list(books))
    }
  }

  def create: Action[AnyContent] = Action { implicit request =>
    Ok(views.html.books.create(bookForm))
  }

  def save: Action[AnyContent] = Action.async { implicit request =>
    bookForm.bindFromRequest().fold(
      formWithErrors => Future.successful(BadRequest(views.html.books.create(formWithErrors))),
      data => {
        val book = models.Book(title = data.title, author = data.author, description = data.description)
        bookRepository.insert(book).map { _ =>
          Redirect(routes.BookController.list).flashing("success" -> "Book added")
        }
      }
    )
  }
}
