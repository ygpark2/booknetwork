package forms

case class PolicyForm(defaultLoanDays: Int, maxExtensions: Int, dailyOverdueFee: BigDecimal)

object PolicyForm {
  def tupled(defaultLoanDays: Int, maxExtensions: Int, dailyOverdueFee: BigDecimal): PolicyForm =
    PolicyForm(defaultLoanDays, maxExtensions, dailyOverdueFee)

  def unapply(form: PolicyForm): Option[(Int, Int, BigDecimal)] =
    Some((form.defaultLoanDays, form.maxExtensions, form.dailyOverdueFee))
}
