package forms

import models.BookMetadata

case class BookForm(
  title: String,
  author: String,
  description: Option[String],
  metadata: BookMetadata
)
