package controllers

import javax.inject._
import play.api.mvc._
import repositories.{BookRepository, UserRepository}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class InteractionController @Inject()(
  cc: ControllerComponents, 
  bookRepository: BookRepository, 
  userRepository: UserRepository
)(implicit ec: ExecutionContext) extends AbstractController(cc) {

  private def withUser(block: models.User => Future[Result])(implicit request: Request[AnyContent]): Future[Result] = {
    request.session.get("userEmail") match {
      case Some(email) =>
        userRepository.findByEmail(email).flatMap {
          case Some(user) => block(user)
          case None => Future.successful(Redirect(routes.AuthController.login).withNewSession)
        }
      case None =>
        Future.successful(Redirect(routes.AuthController.login).flashing("error" -> "Please login to interact."))
    }
  }

  def like(bookId: Long): Action[AnyContent] = Action.async { implicit request =>
    withUser { user =>
      bookRepository.toggleLike(user.id, bookId).map { _ =>
        Redirect(request.headers.get("Referer").getOrElse(routes.BookController.list.url))
      }
    }
  }

  def repost(bookId: Long): Action[AnyContent] = Action.async { implicit request =>
    withUser { user =>
      bookRepository.toggleRepost(user.id, bookId).map { _ =>
        Redirect(request.headers.get("Referer").getOrElse(routes.BookController.list.url))
      }
    }
  }

  def bookmark(bookId: Long): Action[AnyContent] = Action.async { implicit request =>
    withUser { user =>
      bookRepository.toggleBookmark(user.id, bookId).map { _ =>
        Redirect(request.headers.get("Referer").getOrElse(routes.BookController.list.url))
      }
    }
  }

  def comment(bookId: Long): Action[AnyContent] = Action.async { implicit request =>
    withUser { user =>
      val data = request.body.asFormUrlEncoded
      val content = data.flatMap(_.get("content")).flatMap(_.headOption).getOrElse("")
      val parentId = data.flatMap(_.get("parentId")).flatMap(_.headOption).filter(_.nonEmpty).map(_.toLong)
      
      if (content.trim.nonEmpty) {
        bookRepository.addComment(user.id, bookId, content, parentId).map { _ =>
          Redirect(routes.BookController.detail(bookId)).flashing("success" -> "Reply posted.")
        }
      } else {
        Future.successful(Redirect(routes.BookController.detail(bookId)).flashing("error" -> "Reply cannot be empty."))
      }
    }
  }
}
