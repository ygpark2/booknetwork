package controllers

import forms.LoanForm
import models.Loan
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesAbstractController, MessagesControllerComponents}
import repositories.{BookRepository, UserRepository}

import java.time.LocalDate
import javax.inject._
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class LoanController @Inject()(cc: MessagesControllerComponents, bookRepository: BookRepository, userRepository: UserRepository)(implicit ec: ExecutionContext)
  extends MessagesAbstractController(cc) with I18nSupport {

  private val loanForm: Form[LoanForm] = Form(
    mapping(
      "bookItemId" -> longNumber,
      "borrowerEmail" -> email
    )(LoanForm.tupled)(LoanForm.unapply)
  )

  private def requireAuth[A](block: => Future[play.api.mvc.Result])(implicit request: play.api.mvc.Request[A]): Future[play.api.mvc.Result] = {
    request.session.get("userEmail") match {
      case Some(_) => block
      case None => Future.successful(Unauthorized("You must be authenticated to access this page."))
    }
  }

  def index: Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      request.session.get("userEmail") match {
        case Some(email) =>
          userRepository.findByEmail(email).flatMap {
            case Some(user) =>
              val loansF = bookRepository.listLoansByOwner(user.id)
              val itemsF = bookRepository.findItemsByOwner(user.id)
              for {
                loans <- loansF
                items <- itemsF
              } yield Ok(views.html.loans.index(loanForm, loans, items))
            case None =>
              Future.successful(Unauthorized("User not found."))
          }
        case None =>
          Future.successful(Unauthorized("You must be authenticated to access this page."))
      }
    }
  }

  def checkout: Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      loanForm.bindFromRequest().fold(
        formWithErrors => Future.successful(BadRequest(views.html.loans.index(formWithErrors, Seq.empty, Seq.empty))),
        data =>
          request.session.get("userEmail") match {
            case Some(email) =>
              userRepository.findByEmail(email).flatMap {
                case Some(owner) =>
                  userRepository.findByEmail(data.borrowerEmail).flatMap {
                    case Some(borrower) =>
                      bookRepository.findItemById(data.bookItemId).flatMap {
                        case Some(item) =>
                          val loan = Loan(
                            ownerId = owner.id,
                            borrowerId = borrower.id,
                            bookItemId = item.id,
                            loanedAt = LocalDate.now(),
                            dueAt = LocalDate.now().plusDays(7),
                            returnedAt = None,
                            extensionsUsed = 0,
                            status = "ACTIVE"
                          )
                          bookRepository.insertLoan(loan).flatMap { _ =>
                            bookRepository.updateItemStatus(item.id, "LOANED").map { _ =>
                              Redirect(routes.LoanController.index).flashing("success" -> "Loan created")
                            }
                          }
                        case None =>
                          Future.successful(BadRequest("Book item not found"))
                      }
                    case None =>
                      Future.successful(BadRequest("Borrower not found"))
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

  def returnLoan(id: Long): Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      bookRepository.returnLoan(id, LocalDate.now(), "RETURNED").map { _ =>
        Redirect(routes.LoanController.index).flashing("success" -> "Loan returned")
      }
    }
  }
}
