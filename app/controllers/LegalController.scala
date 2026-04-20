package controllers

import javax.inject._
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesAbstractController, MessagesControllerComponents}
import services.SidebarDataService

import scala.concurrent.ExecutionContext

@Singleton
class LegalController @Inject()(cc: MessagesControllerComponents, sidebarDataService: SidebarDataService)(implicit ec: ExecutionContext)
  extends MessagesAbstractController(cc) with I18nSupport {

  def terms: Action[AnyContent] = Action.async { implicit request =>
    for {
      trendingBooks <- sidebarDataService.trendingBooks()
      recommendedUsers <- sidebarDataService.recommendedUsers(request.session.get("userEmail"))
    } yield {
      Ok(views.html.legal.terms(trendingBooks = trendingBooks, recommendedUsers = recommendedUsers))
    }
  }

  def privacy: Action[AnyContent] = Action.async { implicit request =>
    for {
      trendingBooks <- sidebarDataService.trendingBooks()
      recommendedUsers <- sidebarDataService.recommendedUsers(request.session.get("userEmail"))
    } yield {
      Ok(views.html.legal.privacy(trendingBooks = trendingBooks, recommendedUsers = recommendedUsers))
    }
  }
}
