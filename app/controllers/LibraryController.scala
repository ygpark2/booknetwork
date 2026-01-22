package controllers

import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesAbstractController, MessagesControllerComponents}
import repositories.{BookRepository, UserRepository}

import javax.inject._
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class LibraryController @Inject()(cc: MessagesControllerComponents, bookRepository: BookRepository, userRepository: UserRepository)(implicit ec: ExecutionContext)
  extends MessagesAbstractController(cc) with I18nSupport {

  private def requireAuth[A](block: => Future[play.api.mvc.Result])(implicit request: play.api.mvc.Request[A]): Future[play.api.mvc.Result] = {
    request.session.get("userEmail") match {
      case Some(_) => block
      case None => Future.successful(Unauthorized("You must be authenticated to access this page."))
    }
  }

  def dashboard: Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      request.session.get("userEmail") match {
        case Some(email) =>
          userRepository.findByEmail(email).flatMap {
            case Some(user) =>
              bookRepository.listLoansByBorrower(user.id).map { loans =>
                Ok(views.html.library.dashboard(loans))
              }
            case None =>
              Future.successful(Unauthorized("User not found."))
          }
        case None =>
          Future.successful(Unauthorized("You must be authenticated to access this page."))
      }
    }
  }
}
