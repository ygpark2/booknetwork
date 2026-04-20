package forms

import models.BookMetadata

case class BookForm(
  title: String,
  author: String,
  description: Option[String],
  reviewHeadline: Option[String],
  readingStatus: String,
  rating: Option[Int],
  metadata: BookMetadata
)

object BookForm {
  def apply(title: String, author: String, description: Option[String], reviewHeadline: Option[String], readingStatus: String, rating: Option[Int], metadata: BookMetadata): BookForm =
    new BookForm(title, author, description, reviewHeadline, readingStatus, rating, metadata)

  def unapply(form: BookForm): Option[(String, String, Option[String], Option[String], String, Option[Int], BookMetadata)] =
    Some((form.title, form.author, form.description, form.reviewHeadline, form.readingStatus, form.rating, form.metadata))
}
