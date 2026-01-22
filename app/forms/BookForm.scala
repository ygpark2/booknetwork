package forms

import models.BookMetadata

case class BookForm(
  title: String,
  author: String,
  description: Option[String],
  metadata: BookMetadata
)

object BookForm {
  def apply(title: String, author: String, description: Option[String], metadata: BookMetadata): BookForm =
    new BookForm(title, author, description, metadata)

  def unapply(form: BookForm): Option[(String, String, Option[String], BookMetadata)] =
    Some((form.title, form.author, form.description, form.metadata))
}
