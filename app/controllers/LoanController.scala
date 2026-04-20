package controllers

import forms.{LoanForm, LoanPaymentForm}
import models.Loan
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesAbstractController, MessagesControllerComponents}
import repositories.{BookRepository, UserRepository}
import services.SidebarDataService

import java.time.LocalDate
import javax.inject._
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class LoanController @Inject()(cc: MessagesControllerComponents, bookRepository: BookRepository, userRepository: UserRepository, sidebarDataService: SidebarDataService)(implicit ec: ExecutionContext)
  extends MessagesAbstractController(cc) with I18nSupport {

  private val loanForm: Form[LoanForm] = Form(
    mapping(
      "bookItemId" -> longNumber,
      "borrowerEmail" -> email
    )(LoanForm.tupled)(LoanForm.unapply)
  )

  private val loanPaymentForm: Form[LoanPaymentForm] = Form(
    mapping(
      "amount" -> bigDecimal.verifying("Payment amount must be greater than zero", _ > 0)
    )(LoanPaymentForm.tupled)(LoanPaymentForm.unapply)
  )

  private def requireAuth[A](block: => Future[play.api.mvc.Result])(implicit request: play.api.mvc.Request[A]): Future[play.api.mvc.Result] = {
    request.session.get("userEmail") match {
      case Some(_) => block
      case None => Future.successful(Redirect(routes.AuthController.login).flashing("error" -> "You must be authenticated to access this page."))
    }
  }

  def index: Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      request.session.get("userEmail") match {
        case Some(email) =>
          userRepository.findByEmail(email).flatMap {
            case Some(user) =>
              val loansF = bookRepository.listLoanViewDataByOwner(user.id)
              val itemsF = bookRepository.findItemsByOwner(user.id)
              val paymentsF = loansF.flatMap(loans => bookRepository.listLoanPaymentsByLoanIds(loans.map(_.loan.id)))
              val trendingF = sidebarDataService.trendingBooks()
              val recommendedF = sidebarDataService.recommendedUsers(request.session.get("userEmail"))
              for {
                loans <- loansF
                items <- itemsF
                paymentsByLoan <- paymentsF
                trendingBooks <- trendingF
                recommendedUsers <- recommendedF
              } yield Ok(views.html.loans.index(loanForm, loans, paymentsByLoan, items, trendingBooks = trendingBooks, recommendedUsers = recommendedUsers))
            case None =>
              Future.successful(Redirect(routes.AuthController.login).withNewSession.flashing("error" -> "User session invalid, please login again."))
          }
        case None =>
          Future.successful(Redirect(routes.AuthController.login).flashing("error" -> "You must be authenticated to access this page."))
      }
    }
  }

  def checkout: Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      loanForm.bindFromRequest().fold(
        formWithErrors =>
          request.session.get("userEmail") match {
            case Some(email) =>
              userRepository.findByEmail(email).flatMap {
                case Some(user) =>
                  for {
                    loans <- bookRepository.listLoanViewDataByOwner(user.id)
                    items <- bookRepository.findItemsByOwner(user.id)
                    paymentsByLoan <- bookRepository.listLoanPaymentsByLoanIds(loans.map(_.loan.id))
                    trendingBooks <- sidebarDataService.trendingBooks()
                    recommendedUsers <- sidebarDataService.recommendedUsers(request.session.get("userEmail"))
                  } yield BadRequest(views.html.loans.index(formWithErrors, loans, paymentsByLoan, items, trendingBooks = trendingBooks, recommendedUsers = recommendedUsers))
                case None =>
                  Future.successful(Redirect(routes.AuthController.login).withNewSession.flashing("error" -> "User session invalid, please login again."))
              }
            case None =>
              Future.successful(Redirect(routes.AuthController.login).flashing("error" -> "You must be authenticated to access this page."))
          },
        data =>
          request.session.get("userEmail") match {
            case Some(email) =>
              userRepository.findByEmail(email).flatMap {
                case Some(owner) =>
                  userRepository.findPolicyByOwner(owner.id).flatMap { policyOpt =>
                    userRepository.findByEmail(data.borrowerEmail).flatMap {
                      case Some(borrower) =>
                        bookRepository.findItemById(data.bookItemId).flatMap {
                          case Some(item) =>
                            if (borrower.id == owner.id) {
                              Future.successful(Redirect(routes.LoanController.index).flashing("error" -> "You cannot loan an item to yourself."))
                            } else if (item.status != "AVAILABLE") {
                              Future.successful(Redirect(routes.LoanController.index).flashing("error" -> "Only available items can be checked out."))
                            } else {
                              val loanDays = policyOpt.map(_.defaultLoanDays).getOrElse(7)
                              val loan = Loan(
                                ownerId = owner.id,
                                borrowerId = borrower.id,
                                bookItemId = item.id,
                                loanedAt = LocalDate.now(),
                                dueAt = LocalDate.now().plusDays(loanDays.toLong),
                                returnedAt = None,
                                extensionsUsed = 0,
                                status = "ACTIVE"
                              )
                              bookRepository.insertLoan(loan).flatMap { _ =>
                                bookRepository.updateItemStatus(item.id, "LOANED").map { _ =>
                                  Redirect(routes.LoanController.index).flashing("success" -> s"Loan created for $loanDays days")
                                }
                              }
                            }
                          case None =>
                            Future.successful(Redirect(routes.LoanController.index).flashing("error" -> "Book item not found"))
                        }
                      case None =>
                        Future.successful(Redirect(routes.LoanController.index).flashing("error" -> "Borrower not found"))
                    }
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

  def returnLoan(id: Long): Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      bookRepository.returnLoanAndUpdateItemStatus(id, LocalDate.now(), "RETURNED", "AVAILABLE").map {
        case true => Redirect(routes.LoanController.index).flashing("success" -> "Loan returned")
        case false => Redirect(routes.LoanController.index).flashing("error" -> "Loan not found")
      }
    }
  }

  def extendLoan(id: Long): Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      request.session.get("userEmail") match {
        case Some(email) =>
          userRepository.findByEmail(email).flatMap {
            case Some(owner) =>
              userRepository.findPolicyByOwner(owner.id).flatMap {
                case Some(policy) =>
                  bookRepository.findLoanById(id).flatMap {
                    case Some(loan) if loan.ownerId == owner.id =>
                      bookRepository.extendLoan(id, policy.defaultLoanDays, policy.maxExtensions).map {
                        case true => Redirect(routes.LoanController.index).flashing("success" -> s"Loan extended by ${policy.defaultLoanDays} days")
                        case false => Redirect(routes.LoanController.index).flashing("error" -> "Loan cannot be extended")
                      }
                    case Some(_) =>
                      Future.successful(Redirect(routes.LoanController.index).flashing("error" -> "You can only extend your own loans"))
                    case None =>
                      Future.successful(Redirect(routes.LoanController.index).flashing("error" -> "Loan not found"))
                  }
                case None =>
                  Future.successful(Redirect(routes.PolicyController.edit).flashing("error" -> "Set your loan policy before extending loans"))
              }
            case None =>
              Future.successful(Redirect(routes.AuthController.login).withNewSession.flashing("error" -> "User session invalid, please login again."))
          }
        case None =>
          Future.successful(Redirect(routes.AuthController.login).flashing("error" -> "You must be authenticated to access this page."))
      }
    }
  }

  def settleOverdueFee(id: Long): Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      request.session.get("userEmail") match {
        case Some(email) =>
          userRepository.findByEmail(email).flatMap {
            case Some(user) =>
              bookRepository.findLoanById(id).flatMap {
                case Some(loan) if loan.borrowerId == user.id =>
                  bookRepository.settleOverdueFee(id, LocalDate.now()).map {
                    case true => Redirect(routes.LibraryController.dashboard).flashing("success" -> "Overdue fee settled")
                    case false => Redirect(routes.LibraryController.dashboard).flashing("error" -> "No overdue fee to settle")
                  }
                case Some(_) =>
                  Future.successful(Redirect(routes.LibraryController.dashboard).flashing("error" -> "You can only settle your own loan fees"))
                case None =>
                  Future.successful(Redirect(routes.LibraryController.dashboard).flashing("error" -> "Loan not found"))
              }
            case None =>
              Future.successful(Redirect(routes.AuthController.login).withNewSession.flashing("error" -> "User session invalid, please login again."))
          }
        case None =>
          Future.successful(Redirect(routes.AuthController.login).flashing("error" -> "You must be authenticated to access this page."))
      }
    }
  }

  def payOverdueFee(id: Long): Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      loanPaymentForm.bindFromRequest().fold(
        _ => Future.successful(Redirect(routes.LibraryController.dashboard).flashing("error" -> "Enter a valid payment amount")),
        data =>
          request.session.get("userEmail") match {
            case Some(email) =>
              userRepository.findByEmail(email).flatMap {
                case Some(user) =>
                  bookRepository.findLoanById(id).flatMap {
                    case Some(loan) if loan.borrowerId == user.id =>
                      bookRepository.payOverdueFee(id, data.amount, LocalDate.now()).map {
                        case true => Redirect(routes.LibraryController.dashboard).flashing("success" -> "Payment recorded")
                        case false => Redirect(routes.LibraryController.dashboard).flashing("error" -> "Payment exceeds the outstanding fee or no balance is due")
                      }
                    case Some(_) =>
                      Future.successful(Redirect(routes.LibraryController.dashboard).flashing("error" -> "You can only pay your own loan fees"))
                    case None =>
                      Future.successful(Redirect(routes.LibraryController.dashboard).flashing("error" -> "Loan not found"))
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
