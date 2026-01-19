package controllers

import play.api.data._
import play.api.data.Forms._
import models._
import views._
import play.api.mvc._
import play.api.i18n.I18nSupport
import javax.inject._

@Singleton
class Application @Inject() (
  val controllerComponents: ControllerComponents
) extends BaseController with I18nSupport {

  val loginForm = Form(
    mapping(
      "email" -> email,
      "password" -> text
    )((email, password) => (email, password))(_.map(u => (u._1, "")))
      .verifying("Invalid email or password", result => result._1 == "admin@example.com" && result._2 == "password")
  )

  def login: Action[AnyContent] = Action { implicit request =>
    Ok(html.login(loginForm))
  }

  def logout: Action[AnyContent] = Action { implicit request =>
    Redirect(routes.Application.login()).flashing("success" -> "You've been logged out")
  }

  def authenticate: Action[AnyContent] = Action { implicit request =>
    loginForm.bindFromRequest().fold(
      formWithErrors => BadRequest(html.login(formWithErrors)),
      user => Redirect(routes.CoffeesController.index).flashing("success" -> "Logged in")
    )
  }
}
