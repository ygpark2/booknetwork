package models

import java.time.LocalDateTime
import slick.jdbc.H2Profile.api._
import slick.lifted.ProvenShape

case class Book(id: Long = 0L, title: String, author: String, description: Option[String])

class BooksTable(tag: Tag) extends Table[Book](tag, "BOOKS") {
  def id: Rep[Long] = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def title: Rep[String] = column[String]("TITLE")
  def author: Rep[String] = column[String]("AUTHOR")
  def description: Rep[Option[String]] = column[Option[String]]("DESCRIPTION")

  override def * : ProvenShape[Book] = (id, title, author, description).mapTo[Book]
}
