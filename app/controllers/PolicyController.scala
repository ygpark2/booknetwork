package controllers

import forms.PolicyForm
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesAbstractController, MessagesControllerComponents}
import repositories.UserRepository

import javax.inject._
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class PolicyController @Inject()(cc: MessagesControllerComponents, userRepository: UserRepository, bookRepository: repositories.BookRepository)(implicit ec: ExecutionContext)
  extends MessagesAbstractController(cc) with I18nSupport {

  private val policyForm: Form[PolicyForm] = Form(
    mapping(
      "defaultLoanDays" -> number,
      "maxExtensions" -> number,
      "dailyOverdueFee" -> bigDecimal
    )(PolicyForm.tupled)(PolicyForm.unapply)
  )

  private def requireAuth[A](block: => Future[play.api.mvc.Result])(implicit request: play.api.mvc.Request[A]): Future[play.api.mvc.Result] = {
    request.session.get("userEmail") match {
      case Some(_) => block
      case None => Future.successful(Redirect(routes.AuthController.login).flashing("error" -> "You must be authenticated to access this page."))
    }
  }

  def edit: Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      request.session.get("userEmail") match {
        case Some(email) =>
          userRepository.findByEmail(email).flatMap {
            case Some(user) =>
              val policyF = userRepository.findPolicyByOwner(user.id)
              val trendingF = bookRepository.trending()
              for {
                policyOpt <- policyF
                trendingBooks <- trendingF
              } yield {
                policyOpt match {
                  case Some(policy) =>
                    val filled = policyForm.fill(PolicyForm(policy.defaultLoanDays, policy.maxExtensions, policy.dailyOverdueFee))
                    Ok(views.html.policies.edit(filled, trendingBooks = trendingBooks))
                  case None =>
                    Ok(views.html.policies.edit(policyForm, trendingBooks = trendingBooks))
                }
              }
            case None =>
              Future.successful(Redirect(routes.AuthController.login).withNewSession.flashing("error" -> "User session invalid, please login again."))
          }
        case None =>
          Future.successful(Redirect(routes.AuthController.login).flashing("error" -> "You must be authenticated to access this page."))
      }
    }
  }

  def update: Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      policyForm.bindFromRequest().fold(
        formWithErrors => bookRepository.trending().map { trendingBooks =>
          BadRequest(views.html.policies.edit(formWithErrors, trendingBooks = trendingBooks))
        },
        data =>
          request.session.get("userEmail") match {
            case Some(email) =>
              userRepository.findByEmail(email).flatMap {
                case Some(user) =>
                  userRepository.upsertPolicy(user.id, data.defaultLoanDays, data.maxExtensions, data.dailyOverdueFee).map { _ =>
                    Redirect(routes.PolicyController.edit).flashing("success" -> "Policy updated")
                  }
                case None =>
                  Future.successful(Redirect(routes.AuthController.login).withNewSession.flashing("error" -> "User session invalid, please login again."))
              }
            case None =>
              Future.successful(Redirect(routes.AuthController.login).flashing("error" -> "You must be authenticated to access this page."))
          }
      )
    }
  }
}
