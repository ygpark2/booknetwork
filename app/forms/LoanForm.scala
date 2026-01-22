package forms

case class LoanForm(bookItemId: Long, borrowerEmail: String)

object LoanForm {
  def tupled(bookItemId: Long, borrowerEmail: String): LoanForm =
    LoanForm(bookItemId, borrowerEmail)

  def unapply(form: LoanForm): Option[(Long, String)] =
    Some((form.bookItemId, form.borrowerEmail))
}
