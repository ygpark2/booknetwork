package forms

case class LoginForm(email: String, password: String)

object LoginForm {
  def tupled(email: String, password: String): LoginForm =
    LoginForm(email, password)

  def unapply(form: LoginForm): Option[(String, String)] =
    Some((form.email, form.password))
}
