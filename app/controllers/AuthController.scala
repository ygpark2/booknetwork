package controllers

import forms.{LoginForm, RegisterForm}
import models.User
import play.silhouette.api.util.PasswordInfo
import play.silhouette.password.{BCryptPasswordHasher, BCryptSha256PasswordHasher}
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesAbstractController, MessagesControllerComponents}
import repositories.UserRepository

import javax.inject._
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AuthController @Inject()(cc: MessagesControllerComponents, userRepository: UserRepository)(implicit ec: ExecutionContext)
  extends MessagesAbstractController(cc) with I18nSupport {

  private val passwordHasher = new BCryptSha256PasswordHasher()
  private val legacyPasswordHasher = new BCryptPasswordHasher()

  private val registerForm: Form[RegisterForm] = Form(
    mapping(
      "name" -> nonEmptyText,
      "email" -> email,
      "password" -> nonEmptyText
    )(RegisterForm.tupled)(RegisterForm.unapply)
  )

  private val loginForm: Form[LoginForm] = Form(
    mapping(
      "email" -> email,
      "password" -> nonEmptyText
    )(LoginForm.tupled)(LoginForm.unapply)
  )

  def register: Action[AnyContent] = Action { implicit request =>
    Ok(views.html.auth.register(registerForm))
  }

  def registerSubmit: Action[AnyContent] = Action.async { implicit request =>
    registerForm.bindFromRequest().fold(
      formWithErrors => Future.successful(BadRequest(views.html.auth.register(formWithErrors))),
      data => {
        userRepository.findByEmail(data.email).flatMap {
          case Some(_) =>
            val formWithError = registerForm.fill(data).withGlobalError("Email already exists")
            Future.successful(BadRequest(views.html.auth.register(formWithError)))
          case None =>
            val passwordInfo = passwordHasher.hash(data.password)
            val user = User(
              name = data.name,
              email = data.email,
              passwordHasher = passwordInfo.hasher,
              passwordHash = passwordInfo.password
            )
            userRepository.insert(user).flatMap { userId =>
              val policy = models.LibraryPolicy(
                ownerId = userId,
                defaultLoanDays = 7,
                maxExtensions = 1,
                dailyOverdueFee = BigDecimal(0)
              )
              userRepository.insertLibraryPolicy(policy).map { _ =>
                Redirect(routes.BookController.list)
                  .flashing("success" -> "Account created. Please log in.")
              }
            }
        }
      }
    )
  }

  def login: Action[AnyContent] = Action { implicit request =>
    Ok(views.html.auth.login(loginForm))
  }

  def loginSubmit: Action[AnyContent] = Action.async { implicit request =>
    loginForm.bindFromRequest().fold(
      formWithErrors => Future.successful(BadRequest(views.html.auth.login(formWithErrors))),
      data => {
        userRepository.findByEmail(data.email).map {
          case Some(user) =>
            val passwordInfo = PasswordInfo(user.passwordHasher, user.passwordHash)
            val matches = user.passwordHasher match {
              case hasher if hasher == passwordHasher.id =>
                passwordHasher.matches(passwordInfo, data.password)
              case hasher if hasher == legacyPasswordHasher.id =>
                legacyPasswordHasher.matches(passwordInfo, data.password)
              case _ =>
                false
            }

            if (matches) {
              Redirect(routes.BookController.list)
                .withSession(request.session + ("userEmail" -> user.email))
            } else {
              val formWithError = loginForm.fill(data).withGlobalError("Invalid email or password")
              BadRequest(views.html.auth.login(formWithError))
            }
          case None =>
            val formWithError = loginForm.fill(data).withGlobalError("Invalid email or password")
            BadRequest(views.html.auth.login(formWithError))
        }
      }
    )
  }

  def logout: Action[AnyContent] = Action { implicit request =>
    Redirect(routes.BookController.list).withNewSession
  }
}
