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
class PolicyController @Inject()(cc: MessagesControllerComponents, userRepository: UserRepository)(implicit ec: ExecutionContext)
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
      case None => Future.successful(Unauthorized("You must be authenticated to access this page."))
    }
  }

  def edit: Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      request.session.get("userEmail") match {
        case Some(email) =>
          userRepository.findByEmail(email).flatMap {
            case Some(user) =>
              userRepository.findPolicyByOwner(user.id).map {
                case Some(policy) =>
                  val filled = policyForm.fill(PolicyForm(policy.defaultLoanDays, policy.maxExtensions, policy.dailyOverdueFee))
                  Ok(views.html.policies.edit(filled))
                case None =>
                  Ok(views.html.policies.edit(policyForm))
              }
            case None =>
              Future.successful(Unauthorized("User not found."))
          }
        case None =>
          Future.successful(Unauthorized("You must be authenticated to access this page."))
      }
    }
  }

  def update: Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      policyForm.bindFromRequest().fold(
        formWithErrors => Future.successful(BadRequest(views.html.policies.edit(formWithErrors))),
        data =>
          request.session.get("userEmail") match {
            case Some(email) =>
              userRepository.findByEmail(email).flatMap {
                case Some(user) =>
                  userRepository.upsertPolicy(user.id, data.defaultLoanDays, data.maxExtensions, data.dailyOverdueFee).map { _ =>
                    Redirect(routes.PolicyController.edit).flashing("success" -> "Policy updated")
                  }
                case None =>
                  Future.successful(Unauthorized("User not found."))
              }
            case None =>
              Future.successful(Unauthorized("You must be authenticated to access this page."))
          }
      )
    }
  }
}
