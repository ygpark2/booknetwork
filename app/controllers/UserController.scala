package controllers

import javax.inject._
import play.api.i18n.I18nSupport
import play.api.mvc._
import repositories.UserRepository
import services.SidebarDataService
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class UserController @Inject()(
  cc: MessagesControllerComponents,
  userRepository: UserRepository,
  sidebarDataService: SidebarDataService
)(implicit ec: ExecutionContext)
  extends MessagesAbstractController(cc) with I18nSupport {

  def discover: Action[AnyContent] = Action.async { implicit request =>
    for {
      trendingBooks <- sidebarDataService.trendingBooks()
      recommendedUsers <- sidebarDataService.recommendedUsers(request.session.get("userEmail"), limit = 12)
    } yield Ok(views.html.users.discover(recommendedUsers, trendingBooks = trendingBooks, sidebarRecommendedUsers = recommendedUsers.take(3)))
  }

  def follow(followedId: Long): Action[AnyContent] = Action.async { implicit request =>
    request.session.get("userEmail") match {
      case Some(email) =>
        userRepository.findByEmail(email).flatMap {
          case Some(follower) =>
            if (follower.id == followedId) {
              Future.successful(Redirect(request.headers.get("Referer").getOrElse(routes.BookController.list.url))
                .flashing("error" -> "You cannot follow yourself."))
            } else {
              userRepository.follow(follower.id, followedId).map { _ =>
                Redirect(request.headers.get("Referer").getOrElse(routes.BookController.list.url))
                  .flashing("success" -> "User followed.")
              }
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
