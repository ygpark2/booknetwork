package models

import slick.jdbc.H2Profile.api._
import slick.lifted.ProvenShape

case class BookMetadata(
  subtitle: Option[String] = None,
  isbn10: Option[String] = None,
  isbn13: Option[String] = None,
  issn: Option[String] = None,
  publisher: Option[String] = None,
  publicationYear: Option[Int] = None,
  publicationPlace: Option[String] = None,
  edition: Option[String] = None,
  seriesTitle: Option[String] = None,
  seriesNumber: Option[String] = None,
  language: Option[String] = None,
  originalLanguage: Option[String] = None,
  subjects: Option[String] = None,
  keywords: Option[String] = None,
  summary: Option[String] = None,
  classification: Option[String] = None,
  callNumberBase: Option[String] = None,
  format: Option[String] = None,
  pages: Option[Int] = None,
  dimensions: Option[String] = None,
  coverImageUrl: Option[String] = None,
  thumbnailUrl: Option[String] = None
)

object BookMetadata {
  type Tupled = (
    Option[String],
    Option[String],
    Option[String],
    Option[String],
    Option[String],
    Option[Int],
    Option[String],
    Option[String],
    Option[String],
    Option[String],
    Option[String],
    Option[String],
    Option[String],
    Option[String],
    Option[String],
    Option[String],
    Option[String],
    Option[String],
    Option[Int],
    Option[String],
    Option[String],
    Option[String]
  )

  def fromTuple(tuple: Tupled): BookMetadata =
    BookMetadata(
      subtitle = tuple._1,
      isbn10 = tuple._2,
      isbn13 = tuple._3,
      issn = tuple._4,
      publisher = tuple._5,
      publicationYear = tuple._6,
      publicationPlace = tuple._7,
      edition = tuple._8,
      seriesTitle = tuple._9,
      seriesNumber = tuple._10,
      language = tuple._11,
      originalLanguage = tuple._12,
      subjects = tuple._13,
      keywords = tuple._14,
      summary = tuple._15,
      classification = tuple._16,
      callNumberBase = tuple._17,
      format = tuple._18,
      pages = tuple._19,
      dimensions = tuple._20,
      coverImageUrl = tuple._21,
      thumbnailUrl = tuple._22
    )

  def toTuple(metadata: BookMetadata): Option[Tupled] =
    Some(
      (
        metadata.subtitle,
        metadata.isbn10,
        metadata.isbn13,
        metadata.issn,
        metadata.publisher,
        metadata.publicationYear,
        metadata.publicationPlace,
        metadata.edition,
        metadata.seriesTitle,
        metadata.seriesNumber,
        metadata.language,
        metadata.originalLanguage,
        metadata.subjects,
        metadata.keywords,
        metadata.summary,
        metadata.classification,
        metadata.callNumberBase,
        metadata.format,
        metadata.pages,
        metadata.dimensions,
        metadata.coverImageUrl,
        metadata.thumbnailUrl
      )
    )
}

case class Book(
  id: Long = 0L,
  ownerId: Long,
  title: String,
  author: String,
  description: Option[String],
  metadata: BookMetadata = BookMetadata()
)

class BooksTable(tag: Tag) extends Table[Book](tag, "BOOKS") {
  def id: Rep[Long] = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def ownerId: Rep[Long] = column[Long]("OWNER_ID")
  def title: Rep[String] = column[String]("TITLE")
  def author: Rep[String] = column[String]("AUTHOR")
  def description: Rep[Option[String]] = column[Option[String]]("DESCRIPTION")
  def subtitle: Rep[Option[String]] = column[Option[String]]("SUBTITLE")
  def isbn10: Rep[Option[String]] = column[Option[String]]("ISBN10")
  def isbn13: Rep[Option[String]] = column[Option[String]]("ISBN13")
  def issn: Rep[Option[String]] = column[Option[String]]("ISSN")
  def publisher: Rep[Option[String]] = column[Option[String]]("PUBLISHER")
  def publicationYear: Rep[Option[Int]] = column[Option[Int]]("PUBLICATION_YEAR")
  def publicationPlace: Rep[Option[String]] = column[Option[String]]("PUBLICATION_PLACE")
  def edition: Rep[Option[String]] = column[Option[String]]("EDITION")
  def seriesTitle: Rep[Option[String]] = column[Option[String]]("SERIES_TITLE")
  def seriesNumber: Rep[Option[String]] = column[Option[String]]("SERIES_NUMBER")
  def language: Rep[Option[String]] = column[Option[String]]("LANGUAGE")
  def originalLanguage: Rep[Option[String]] = column[Option[String]]("ORIGINAL_LANGUAGE")
  def subjects: Rep[Option[String]] = column[Option[String]]("SUBJECTS")
  def keywords: Rep[Option[String]] = column[Option[String]]("KEYWORDS")
  def summary: Rep[Option[String]] = column[Option[String]]("SUMMARY")
  def classification: Rep[Option[String]] = column[Option[String]]("CLASSIFICATION")
  def callNumberBase: Rep[Option[String]] = column[Option[String]]("CALL_NUMBER_BASE")
  def format: Rep[Option[String]] = column[Option[String]]("FORMAT")
  def pages: Rep[Option[Int]] = column[Option[Int]]("PAGES")
  def dimensions: Rep[Option[String]] = column[Option[String]]("DIMENSIONS")
  def coverImageUrl: Rep[Option[String]] = column[Option[String]]("COVER_IMAGE_URL")
  def thumbnailUrl: Rep[Option[String]] = column[Option[String]]("THUMBNAIL_URL")

  def owner = foreignKey("FK_BOOK_OWNER", ownerId, TableQuery[UsersTable])(_.id)

  private def metadataColumns = (
    subtitle,
    isbn10,
    isbn13,
    issn,
    publisher,
    publicationYear,
    publicationPlace,
    edition,
    seriesTitle,
    seriesNumber,
    language,
    originalLanguage,
    subjects,
    keywords,
    summary,
    classification,
    callNumberBase,
    format,
    pages,
    dimensions,
    coverImageUrl,
    thumbnailUrl
  )

  def metadata = metadataColumns <> (BookMetadata.fromTuple, BookMetadata.toTuple)

  private def bookTupled(tuple: (Long, Long, String, String, Option[String], BookMetadata)): Book =
    Book(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6)

  private def bookUntupled(book: Book): Option[(Long, Long, String, String, Option[String], BookMetadata)] =
    Some((book.id, book.ownerId, book.title, book.author, book.description, book.metadata))

  override def * : ProvenShape[Book] = (id, ownerId, title, author, description, metadata) <> (bookTupled, bookUntupled)
}
