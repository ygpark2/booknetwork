package forms

case class BookForm(title: String, author: String, description: Option[String])

object BookForm {
  def tupled(title: String, author: String, description: Option[String]): BookForm =
    BookForm(title, author, description)
}
