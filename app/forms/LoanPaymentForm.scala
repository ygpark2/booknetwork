package forms

import scala.math.BigDecimal

case class LoanPaymentForm(amount: BigDecimal)

object LoanPaymentForm {
  def tupled(amount: BigDecimal): LoanPaymentForm =
    LoanPaymentForm(amount)

  def unapply(form: LoanPaymentForm): Option[BigDecimal] =
    Some(form.amount)
}
