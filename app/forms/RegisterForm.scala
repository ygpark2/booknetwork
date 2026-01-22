package forms

case class RegisterForm(name: String, email: String, password: String)

object RegisterForm {
  def tupled(name: String, email: String, password: String): RegisterForm =
    RegisterForm(name, email, password)

  def unapply(form: RegisterForm): Option[(String, String, String)] =
    Some((form.name, form.email, form.password))
}
