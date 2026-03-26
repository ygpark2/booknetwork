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
      case None => Future.successful(Redirect(routes.AuthController.login).flashing("error" -> "You must be authenticated to access this page."))
    }
  }

  def dashboard: Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      request.session.get("userEmail") match {
        case Some(email) =>
          userRepository.findByEmail(email).flatMap {
            case Some(user) =>
              for {
                loans <- bookRepository.listLoansByBorrower(user.id)
                trendingBooks <- bookRepository.trending()
              } yield Ok(views.html.library.dashboard(loans, trendingBooks = trendingBooks))
            case None =>
              Future.successful(Redirect(routes.AuthController.login).withNewSession.flashing("error" -> "User session invalid, please login again."))
          }
        case None =>
          Future.successful(Redirect(routes.AuthController.login).flashing("error" -> "You must be authenticated to access this page."))
      }
    }
  }
}
