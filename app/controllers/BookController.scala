package controllers

import play.api.data._
import play.api.data.Forms._
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesAbstractController, MessagesControllerComponents}
import repositories.{BookRepository, UserRepository}
import services.SidebarDataService

import forms.BookForm

import javax.inject._
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class BookController @Inject()(cc: MessagesControllerComponents, bookRepository: BookRepository, userRepository: UserRepository, sidebarDataService: SidebarDataService)(implicit ec: ExecutionContext)
  extends MessagesAbstractController(cc) with I18nSupport {

  val bookForm: Form[BookForm] = Form(
    mapping(
      "title" -> nonEmptyText,
      "author" -> nonEmptyText,
      "description" -> optional(text),
      "reviewHeadline" -> optional(text),
      "readingStatus" -> nonEmptyText,
      "rating" -> optional(number(min = 1, max = 5)),
      "metadata" -> mapping(
        "subtitle" -> optional(text),
        "isbn10" -> optional(text),
        "isbn13" -> optional(text),
        "issn" -> optional(text),
        "publisher" -> optional(text),
        "publicationYear" -> optional(number),
        "publicationPlace" -> optional(text),
        "edition" -> optional(text),
        "seriesTitle" -> optional(text),
        "seriesNumber" -> optional(text),
        "language" -> optional(text),
        "originalLanguage" -> optional(text),
        "subjects" -> optional(text),
        "keywords" -> optional(text),
        "summary" -> optional(text),
        "classification" -> optional(text),
        "callNumberBase" -> optional(text),
        "format" -> optional(text),
        "pages" -> optional(number),
        "dimensions" -> optional(text),
        "coverImageUrl" -> optional(text),
        "thumbnailUrl" -> optional(text)
      )(
        (
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
        ) => models.BookMetadata(
          subtitle = subtitle,
          isbn10 = isbn10,
          isbn13 = isbn13,
          issn = issn,
          publisher = publisher,
          publicationYear = publicationYear,
          publicationPlace = publicationPlace,
          edition = edition,
          seriesTitle = seriesTitle,
          seriesNumber = seriesNumber,
          language = language,
          originalLanguage = originalLanguage,
          subjects = subjects,
          keywords = keywords,
          summary = summary,
          classification = classification,
          callNumberBase = callNumberBase,
          format = format,
          pages = pages,
          dimensions = dimensions,
          coverImageUrl = coverImageUrl,
          thumbnailUrl = thumbnailUrl
        )
      )(
        metadata => Some(
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
      )
    )(BookForm.apply)(BookForm.unapply)
  )

  private val allowedReadingStatuses = Set("WANT_TO_READ", "READING", "FINISHED", "REREADING")

  private def normalizedMetadata(metadata: models.BookMetadata): models.BookMetadata =
    metadata.copy(
      isbn10 = metadata.isbn10.map(normalizeIsbn).filter(_.nonEmpty),
      isbn13 = metadata.isbn13.map(normalizeIsbn).filter(_.nonEmpty)
    )

  private def normalizeIsbn(isbn: String): String =
    isbn.toUpperCase.replaceAll("[^0-9X]", "")


  def list: Action[AnyContent] = Action.async { implicit request =>
    val userEmailOpt = request.session.get("userEmail")
    val userFuture = userEmailOpt match {
      case Some(email) => userRepository.findByEmail(email)
      case None => Future.successful(None)
    }

    userFuture.flatMap { userOpt =>
      bookRepository.list().flatMap { books =>
        for {
          trendingBooks <- sidebarDataService.trendingBooks()
          recommendedUsers <- sidebarDataService.recommendedUsers(request.session.get("userEmail"))
          booksWithData <- fetchViewData(books, userOpt)
        } yield {
            implicit val messages = request.messages
            Ok(views.html.books.list(booksWithData, trendingBooks = trendingBooks, recommendedUsers = recommendedUsers))
        }
      }
    }
  }

  def bookmarkList: Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      val email = request.session.get("userEmail").get
      userRepository.findByEmail(email).flatMap {
        case Some(user) =>
          bookRepository.listBookmarks(user.id).flatMap { books =>
            for {
              trendingBooks <- sidebarDataService.trendingBooks()
              recommendedUsers <- sidebarDataService.recommendedUsers(request.session.get("userEmail"))
              booksWithData <- fetchViewData(books, Some(user))
            } yield {
                implicit val messages = request.messages
                Ok(views.html.books.list(booksWithData, "Bookmarks", trendingBooks = trendingBooks, recommendedUsers = recommendedUsers))
            }
          }
        case None => Future.successful(Redirect(routes.AuthController.login))
      }
    }
  }

  private def fetchViewData(books: Seq[models.Book], userOpt: Option[models.User])(implicit request: play.api.mvc.RequestHeader): Future[Seq[models.BookViewData]] = {
    Future.sequence(books.map { book =>
      for {
        stats <- bookRepository.getInteractionStats(book.id)
        userInteractions <- userOpt match {
          case Some(user) => bookRepository.getUserInteractions(book.id, user.id)
          case None => Future.successful((false, false, false))
        }
      } yield models.BookViewData(
        book = book,
        likeCount = stats._1,
        repostCount = stats._2,
        bookmarkCount = stats._3,
        commentCount = stats._4,
        isLiked = userInteractions._1,
        isReposted = userInteractions._2,
        isBookmarked = userInteractions._3,
        isOwner = userOpt.exists(_.id == book.ownerId)
      )
    })
  }

  private def requireAuth[A](block: => Future[play.api.mvc.Result])(implicit request: play.api.mvc.Request[A]): Future[play.api.mvc.Result] = {
    request.session.get("userEmail") match {
      case Some(_) => block
      case None => Future.successful(Redirect(routes.AuthController.login).flashing("error" -> "You must be authenticated to access this page."))
    }
  }

  def create: Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      val prefilledForm = bookForm.fill(
        BookForm(
          title = request.getQueryString("title").getOrElse(""),
          author = request.getQueryString("author").getOrElse(""),
          description = None,
          reviewHeadline = None,
          readingStatus = "FINISHED",
          rating = None,
          metadata = models.BookMetadata(
            isbn13 = request.getQueryString("isbn").map(normalizeIsbn).filter(_.nonEmpty)
          )
        )
      )
      for {
        trendingBooks <- sidebarDataService.trendingBooks()
        recommendedUsers <- sidebarDataService.recommendedUsers(request.session.get("userEmail"))
      } yield {
        Ok(views.html.books.create(prefilledForm, trendingBooks = trendingBooks, recommendedUsers = recommendedUsers))
      }
    }
  }

  private val itemForm = Form(
    mapping(
      "barcode" -> nonEmptyText,
      "callNumber" -> optional(text),
      "location" -> optional(text)
    )((barcode, callNumber, location) => (barcode, callNumber, location))
      ({ form => Some((form._1, form._2, form._3)) })
  )

  private def getBookDetailData(id: Long, userOpt: Option[models.User])(implicit request: play.api.mvc.RequestHeader): Future[Option[(models.BookViewData, Seq[models.BookItem], Seq[(models.BookComment, models.User)])]] = {
    bookRepository.findById(id).flatMap {
      case Some(book) =>
        for {
          items <- bookRepository.findItemsByBookId(id)
          stats <- bookRepository.getInteractionStats(book.id)
          userInteractions <- userOpt match {
            case Some(user) => bookRepository.getUserInteractions(book.id, user.id)
            case None => Future.successful((false, false, false))
          }
          comments <- bookRepository.listComments(book.id)
        } yield Some((
          models.BookViewData(
            book = book,
            likeCount = stats._1,
            repostCount = stats._2,
            bookmarkCount = stats._3,
            commentCount = stats._4,
            isLiked = userInteractions._1,
            isReposted = userInteractions._2,
            isBookmarked = userInteractions._3,
            isOwner = userOpt.exists(_.id == book.ownerId)
          ),
          items,
          comments
        ))
      case None => Future.successful(None)
    }
  }

  private def readingStatusLabel(status: String): String =
    status match {
      case "WANT_TO_READ" => "Want to Read"
      case "READING" => "Reading"
      case "REREADING" => "Re-reading"
      case _ => "Finished"
    }

  def detail(id: Long): Action[AnyContent] = Action.async { implicit request =>
    val userEmailOpt = request.session.get("userEmail")
    val userFuture = userEmailOpt match {
      case Some(email) => userRepository.findByEmail(email)
      case None => Future.successful(None)
    }

    userFuture.flatMap { userOpt =>
      getBookDetailData(id, userOpt).flatMap {
        case Some((viewData, items, comments)) =>
          val relatedByIsbnF = bookRepository.primaryIsbn(viewData.book) match {
            case Some(isbn) =>
              bookRepository.listByIsbn(isbn).flatMap(books => fetchViewData(books.filterNot(_.id == viewData.book.id).take(6), userOpt))
            case None =>
              Future.successful(Seq.empty)
          }
          for {
            trendingBooks <- sidebarDataService.trendingBooks()
            recommendedUsers <- sidebarDataService.recommendedUsers(request.session.get("userEmail"))
            relatedByIsbn <- relatedByIsbnF
          } yield {
            Ok(views.html.books.detail(viewData, items, itemForm, comments, relatedByIsbn, readingStatusLabel(viewData.book.readingStatus), trendingBooks = trendingBooks, recommendedUsers = recommendedUsers))
          }
        case None =>
          Future.successful(NotFound("Book not found"))
      }
    }
  }

  def isbnFeed(isbn: String): Action[AnyContent] = Action.async { implicit request =>
    val userEmailOpt = request.session.get("userEmail")
    val userFuture = userEmailOpt match {
      case Some(email) => userRepository.findByEmail(email)
      case None => Future.successful(None)
    }

    userFuture.flatMap { userOpt =>
      bookRepository.listByIsbn(isbn).flatMap { books =>
        if (books.isEmpty) Future.successful(NotFound("No reader posts found for this ISBN"))
        else {
          for {
            trendingBooks <- sidebarDataService.trendingBooks()
            recommendedUsers <- sidebarDataService.recommendedUsers(request.session.get("userEmail"))
            booksWithData <- fetchViewData(books, userOpt)
          } yield {
            val displayIsbn = bookRepository.primaryIsbn(books.head).getOrElse(normalizeIsbn(isbn))
            Ok(views.html.books.isbn(displayIsbn, books.head.title, books.head.author, booksWithData, trendingBooks = trendingBooks, recommendedUsers = recommendedUsers))
          }
        }
      }
    }
  }

  def edit(id: Long): Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      bookRepository.findById(id).flatMap {
        case Some(book) =>
          val filledForm = bookForm.fill(
            BookForm(
              title = book.title,
              author = book.author,
              description = book.description,
              reviewHeadline = book.reviewHeadline,
              readingStatus = book.readingStatus,
              rating = book.rating,
              metadata = book.metadata
            )
          )
          for {
            trendingBooks <- sidebarDataService.trendingBooks()
            recommendedUsers <- sidebarDataService.recommendedUsers(request.session.get("userEmail"))
          } yield {
            Ok(views.html.books.edit(id, filledForm, trendingBooks = trendingBooks, recommendedUsers = recommendedUsers))
          }
        case None =>
          Future.successful(NotFound("Book not found"))
      }
    }
  }

  def update(id: Long): Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      bookForm.bindFromRequest().fold(
        formWithErrors =>
          for {
            trendingBooks <- sidebarDataService.trendingBooks()
            recommendedUsers <- sidebarDataService.recommendedUsers(request.session.get("userEmail"))
          } yield {
            BadRequest(views.html.books.edit(id, formWithErrors, trendingBooks = trendingBooks, recommendedUsers = recommendedUsers))
          },
        data => {
          request.session.get("userEmail") match {
            case Some(email) =>
              userRepository.findByEmail(email).flatMap {
                case Some(user) =>
                  val readingStatus = if (allowedReadingStatuses.contains(data.readingStatus)) data.readingStatus else "FINISHED"
                  val updatedBook = models.Book(
                    id = id,
                    ownerId = user.id,
                    title = data.title,
                    author = data.author,
                    description = data.description,
                    reviewHeadline = data.reviewHeadline.map(_.trim).filter(_.nonEmpty),
                    readingStatus = readingStatus,
                    rating = data.rating,
                    metadata = normalizedMetadata(data.metadata),
                    createdAt = java.time.LocalDateTime.now()
                  )
                  bookRepository.findById(id).flatMap {
                    case Some(existingBook) =>
                      bookRepository.update(id, updatedBook.copy(createdAt = existingBook.createdAt)).map { _ =>
                        Redirect(routes.BookController.detail(id)).flashing("success" -> "Book updated")
                      }
                    case None =>
                      Future.successful(NotFound("Book not found"))
                  }
                case None =>
                  Future.successful(Redirect(routes.AuthController.login).withNewSession.flashing("error" -> "User session invalid, please login again."))
              }
            case None =>
              Future.successful(Redirect(routes.AuthController.login).flashing("error" -> "You must be authenticated to access this page."))
          }
        }
      )
    }
  }

  def addItem(id: Long): Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      itemForm.bindFromRequest().fold(
        formWithErrors => {
          val email = request.session.get("userEmail").get
          userRepository.findByEmail(email).flatMap { userOpt =>
            getBookDetailData(id, userOpt).flatMap {
              case Some((viewData, items, comments)) =>
                for {
                  trendingBooks <- sidebarDataService.trendingBooks()
                  recommendedUsers <- sidebarDataService.recommendedUsers(request.session.get("userEmail"))
                } yield {
                  BadRequest(views.html.books.detail(viewData, items, formWithErrors, comments, Seq.empty, readingStatusLabel(viewData.book.readingStatus), trendingBooks = trendingBooks, recommendedUsers = recommendedUsers))
                }
              case None =>
                Future.successful(NotFound("Book not found"))
            }
          }
        },
        data =>
          bookRepository.addItem(id, data._1, data._2, data._3).map { _ =>
            Redirect(routes.BookController.detail(id)).flashing("success" -> "Item added")
          }
      )
    }
  }

  def save: Action[AnyContent] = Action.async { implicit request =>
    requireAuth {
      bookForm.bindFromRequest().fold(
        formWithErrors =>
          for {
            trendingBooks <- sidebarDataService.trendingBooks()
            recommendedUsers <- sidebarDataService.recommendedUsers(request.session.get("userEmail"))
          } yield {
            BadRequest(views.html.books.create(formWithErrors, trendingBooks = trendingBooks, recommendedUsers = recommendedUsers))
          },
        data => {
          request.session.get("userEmail") match {
            case Some(email) =>
              userRepository.findByEmail(email).flatMap {
                case Some(user) =>
                  val readingStatus = if (allowedReadingStatuses.contains(data.readingStatus)) data.readingStatus else "FINISHED"
                  val book = models.Book(
                    ownerId = user.id,
                    title = data.title,
                    author = data.author,
                    description = data.description,
                    reviewHeadline = data.reviewHeadline.map(_.trim).filter(_.nonEmpty),
                    readingStatus = readingStatus,
                    rating = data.rating,
                    metadata = normalizedMetadata(data.metadata)
                  )
                  val item = models.BookItem(
                    bookId = 0L,
                    barcode = java.util.UUID.randomUUID().toString
                  )
                  bookRepository.insertWithItem(book, item).map { _ =>
                    Redirect(routes.BookController.list).flashing("success" -> "Book added")
                  }
                case None =>
                  Future.successful(Redirect(routes.AuthController.login).withNewSession.flashing("error" -> "User session invalid, please login again."))
              }
            case None =>
              Future.successful(Redirect(routes.AuthController.login).flashing("error" -> "You must be authenticated to access this page."))
          }
        }
      )
    }
  }
}
