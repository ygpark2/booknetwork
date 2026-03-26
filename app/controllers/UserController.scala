package controllers

import javax.inject._
import play.api.mvc._
import repositories.UserRepository
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class UserController @Inject()(cc: ControllerComponents, userRepository: UserRepository)(implicit ec: ExecutionContext)
  extends AbstractController(cc) {

  def follow(followedId: Long): Action[AnyContent] = Action.async { implicit request =>
    request.session.get("userEmail") match {
      case Some(email) =>
        userRepository.findByEmail(email).flatMap {
          case Some(follower) =>
            userRepository.follow(follower.id, followedId).map { _ =>
              Redirect(request.headers.get("Referer").getOrElse(routes.BookController.list.url))
                .flashing("success" -> "User followed.")
            }
          case None =>
            Future.successful(Redirect(routes.AuthController.login).withNewSession)
        }
      case None =>
        Future.successful(Redirect(routes.AuthController.login))
    }
  }

  def unfollow(followedId: Long): Action[AnyContent] = Action.async { implicit request =>
    request.session.get("userEmail") match {
      case Some(email) =>
        userRepository.findByEmail(email).flatMap {
          case Some(follower) =>
            userRepository.unfollow(follower.id, followedId).map { _ =>
              Redirect(request.headers.get("Referer").getOrElse(routes.BookController.list.url))
                .flashing("success" -> "User unfollowed.")
            }
          case None =>
            Future.successful(Redirect(routes.AuthController.login).withNewSession)
        }
      case None =>
        Future.successful(Redirect(routes.AuthController.login))
    }
  }
}
