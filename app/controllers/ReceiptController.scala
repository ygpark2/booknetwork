package controllers

import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesAbstractController, MessagesControllerComponents}
import repositories.{BookRepository, UserRepository}

import javax.inject._
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ReceiptController @Inject()(
  cc: MessagesControllerComponents,
  bookRepository: BookRepository,
  userRepository: UserRepository
)(implicit ec: ExecutionContext) extends MessagesAbstractController(cc) with I18nSupport {

  private def requireAuth[A](block: models.User => Future[play.api.mvc.Result])(implicit request: play.api.mvc.Request[A]): Future[play.api.mvc.Result] = {
    request.session.get("userEmail") match {
      case Some(email) =>
        userRepository.findByEmail(email).flatMap {
          case Some(user) => block(user)
          case None => Future.successful(Redirect(routes.AuthController.login).withNewSession.flashing("error" -> "User session invalid, please login again."))
        }
      case None =>
        Future.successful(Redirect(routes.AuthController.login).flashing("error" -> "You must be authenticated to access this page."))
    }
  }

  def download(receiptNumber: String): Action[AnyContent] = Action.async { implicit request =>
    requireAuth { user =>
      bookRepository.findLoanPaymentByReceiptNumber(receiptNumber).flatMap {
        case Some(payment) =>
          bookRepository.findLoanById(payment.loanId).flatMap {
            case Some(loan) if loan.ownerId == user.id || loan.borrowerId == user.id =>
              for {
                ownerActual <- if (loan.ownerId == user.id) Future.successful(Some(user)) else userRepository.findById(loan.ownerId)
                borrowerActual <- if (loan.borrowerId == user.id) Future.successful(Some(user)) else userRepository.findById(loan.borrowerId)
                bookItem <- bookRepository.findItemById(loan.bookItemId)
                book <- bookItem match {
                  case Some(item) => bookRepository.findById(item.bookId)
                  case None => Future.successful(None)
                }
              } yield {
                val fileName = s"${receiptNumber}.txt"
                val content =
                  s"""BookNetwork Receipt
                     |Receipt Number: ${payment.receiptNumber}
                     |Paid At: ${payment.paidAt}
                     |Amount: ${payment.amount}
                     |Loan ID: ${loan.id}
                     |Book: ${book.map(_.title).getOrElse("Unknown")}
                     |Author: ${book.map(_.author).getOrElse("Unknown")}
                     |Owner: ${ownerActual.map(_.name).getOrElse("Unknown")}
                     |Borrower: ${borrowerActual.map(_.name).getOrElse("Unknown")}
                     |Status: ${loan.status}
                     |""".stripMargin
                Ok(content)
                  .as("text/plain; charset=utf-8")
                  .withHeaders("Content-Disposition" -> s"""attachment; filename="$fileName"""")
              }
            case Some(_) =>
              Future.successful(Forbidden("You cannot access this receipt"))
            case None =>
              Future.successful(NotFound("Loan not found"))
          }
        case None =>
          Future.successful(NotFound("Receipt not found"))
      }
    }
  }
}
