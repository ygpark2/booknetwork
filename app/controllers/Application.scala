package controllers

import play.api.data._
import play.api.data.Forms._
import play.api.templates._
import models._
import views._
import play.api.mvc._
import play.api.mvc.Results._
import jp.t2v.lab.play2.auth._
import play.api.Play._
import play.api.cache.Cache
import reflect.classTag
import jp.t2v.lab.play2.stackc.{ RequestWithAttributes, RequestAttributeKey, StackableController }

object Application extends Controller with LoginLogout with AuthConfigImpl {

  val loginForm = Form {
    mapping("email" -> email, "password" -> text)(Account.authenticate)(_.map(u => (u.email, "")))
      .verifying("Invalid email or password", result => result.isDefined)
  }

  def login = Action { implicit request =>
    println("start login")
    Ok(html.login(loginForm))
  }

  def logout = Action { implicit request =>
    gotoLogoutSucceeded.flashing(
      "success" -> "You've been logged out")
  }

  def authenticate = Action { implicit request =>
    println("start act")
    loginForm.bindFromRequest.fold(
      formWithErrors => { println("form eror"); BadRequest(html.login(formWithErrors)) },
      user => gotoLoginSucceeded(user.get.id))
  }
}

trait AuthConfigImpl extends AuthConfig {

  type Id = Int

  type User = Account

  type Authority = Permission

  val idTag = classTag[Id]

  val sessionTimeoutInSeconds = 3600

  def resolveUser(id: Id) = Account.findById(id)

  def loginSucceeded(request: RequestHeader) = Redirect(routes.CoffeesController.index)

  def logoutSucceeded(request: RequestHeader) = Redirect(routes.Application.login)

  def authenticationFailed(request: RequestHeader) = Redirect(routes.Application.login)

  def authorizationFailed(request: RequestHeader) = Forbidden("no permission")

  def authorize(user: User, authority: Authority) = (user.permission, authority) match {
    case (Administrator, _) => true
    case (NormalUser, NormalUser) => true
    case _ => false
  }
}
