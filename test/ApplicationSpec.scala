import org.scalatestplus.play._
import org.scalatestplus.play.guice.GuiceOneAppPerTest
import java.time.LocalDate
import models.{Book, BookItem, LibraryPolicy, Loan, User}
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.silhouette.password.BCryptSha256PasswordHasher
import play.api.test.Helpers._
import play.api.test._
import play.api.test.CSRFTokenHelper._
import repositories.{BookRepository, UserRepository}
import tasks.DataInitializer

class ApplicationSpec extends PlaySpec with GuiceOneAppPerTest {

  override def fakeApplication(): Application = {
    val application = new GuiceApplicationBuilder()
      .configure(
        "db.default.url" -> s"jdbc:h2:mem:booknetwork-test-${java.util.UUID.randomUUID()};DB_CLOSE_DELAY=-1",
        "db.default.driver" -> "org.h2.Driver",
        "app.db.threads" -> 1
      )
      .build()
    await(application.injector.instanceOf[DataInitializer].init)
    application
  }

  private def createUser(email: String, name: String = "Test User"): Long = {
    val userRepository = app.injector.instanceOf[UserRepository]
    await(
      userRepository.insert(
        User(
          name = name,
          email = email,
          passwordHasher = "bcrypt-sha256",
          passwordHash = "unused"
        )
      )
    )
  }

  private def createPolicy(ownerId: Long, defaultLoanDays: Int = 7, maxExtensions: Int = 1, dailyOverdueFee: BigDecimal = BigDecimal(0.5)): Long = {
    val userRepository = app.injector.instanceOf[UserRepository]
    await(userRepository.insertLibraryPolicy(LibraryPolicy(ownerId = ownerId, defaultLoanDays = defaultLoanDays, maxExtensions = maxExtensions, dailyOverdueFee = dailyOverdueFee)))
  }

  private def createBookWithItem(ownerId: Long, title: String = "Loanable Book"): (Long, Long) = {
    val bookRepository = app.injector.instanceOf[BookRepository]
    val bookId = await(
      bookRepository.insert(
        Book(
          ownerId = ownerId,
          title = title,
          author = "Author",
          description = Some("For loan tests")
        )
      )
    )
    val itemId = await(bookRepository.addItem(bookId, s"barcode-$bookId", None, None))
    (bookId, itemId)
  }

  "Application" should {

    "register a new user" in {
      val request = addCSRFToken(
        FakeRequest(POST, "/auth/register")
          .withFormUrlEncodedBody(
            "name" -> "Alice Reader",
            "email" -> "alice@example.com",
            "password" -> "secret123"
          )
      )
      val result = route(app, request).value
      val userRepository = app.injector.instanceOf[UserRepository]

      status(result) mustBe SEE_OTHER
      redirectLocation(result) mustBe Some("/books")
      await(userRepository.findByEmail("alice@example.com")).isDefined mustBe true
    }

    "log in with a registered user and create a session" in {
      val userRepository = app.injector.instanceOf[UserRepository]
      val passwordHasher = new BCryptSha256PasswordHasher()
      val passwordInfo = passwordHasher.hash("password123")
      await(
        userRepository.insert(
          User(
            name = "Bob Reader",
            email = "bob@example.com",
            passwordHasher = passwordInfo.hasher,
            passwordHash = passwordInfo.password
          )
        )
      )

      val loginRequest = addCSRFToken(
        FakeRequest(POST, "/auth/login")
          .withFormUrlEncodedBody(
            "email" -> "bob@example.com",
            "password" -> "password123"
          )
      )
      val result = route(app, loginRequest).value

      status(result) mustBe SEE_OTHER
      session(result).get("userEmail") mustBe Some("bob@example.com")
      await(userRepository.findByEmail("bob@example.com")).isDefined mustBe true
    }

    "redirect the root route to books" in {
      val request = FakeRequest(GET, "/")
      val result = route(app, request).value

      status(result) mustBe SEE_OTHER
      redirectLocation(result) mustBe Some("/books")
    }

    "render the terms page" in {
      val request = FakeRequest(GET, "/terms")
      val result = route(app, request).value

      status(result) mustBe OK
      contentAsString(result) must include ("Terms of Service")
    }

    "redirect unauthenticated users away from the library dashboard" in {
      val request = FakeRequest(GET, "/library")
      val result = route(app, request).value

      status(result) mustBe SEE_OTHER
      redirectLocation(result) mustBe Some("/auth/login")
    }

    "allow an authenticated user to follow another user" in {
      val userRepository = app.injector.instanceOf[UserRepository]
      val followerId = await(
        userRepository.insert(
          User(
            name = "Follower",
            email = "follower@example.com",
            passwordHasher = "bcrypt-sha256",
            passwordHash = "unused"
          )
        )
      )
      val followedId = await(
        userRepository.insert(
          User(
            name = "Followed",
            email = "followed@example.com",
            passwordHasher = "bcrypt-sha256",
            passwordHash = "unused"
          )
        )
      )
      val request = addCSRFToken(
        FakeRequest(POST, s"/users/$followedId/follow")
          .withSession("userEmail" -> "follower@example.com")
      )
      val result = route(app, request).value

      status(result) mustBe SEE_OTHER
      await(userRepository.isFollowing(followerId, followedId)) mustBe true
    }

    "allow an authenticated user to submit a book report" in {
      val userRepository = app.injector.instanceOf[UserRepository]
      val bookRepository = app.injector.instanceOf[BookRepository]
      createUser("reporter@example.com", "Reporter")
      val ownerId = createUser("owner@example.com", "Owner")
      createPolicy(ownerId)
      val bookId = await(
        bookRepository.insert(
          Book(
            ownerId = ownerId,
            title = "Test Driven Book",
            author = "Writer",
            description = Some("A seeded test book")
          )
        )
      )

      val request = addCSRFToken(
        FakeRequest(POST, s"/books/$bookId/report")
          .withSession("userEmail" -> "reporter@example.com")
          .withFormUrlEncodedBody("reason" -> "Incorrect metadata")
      )
      val result = route(app, request).value

      status(result) mustBe SEE_OTHER
      await(bookRepository.countReportsForBook(bookId)) mustBe 1
    }

    "allow an authenticated user to like a book" in {
      val bookRepository = app.injector.instanceOf[BookRepository]
      val ownerId = createUser("like-owner@example.com", "Like Owner")
      val likerId = createUser("liker@example.com", "Liker")
      val bookId = await(
        bookRepository.insert(
          Book(
            ownerId = ownerId,
            title = "Likable Book",
            author = "Author",
            description = Some("Like test")
          )
        )
      )

      val request = addCSRFToken(
        FakeRequest(POST, s"/books/$bookId/like")
          .withSession("userEmail" -> "liker@example.com")
      )
      val result = route(app, request).value
      status(result) mustBe SEE_OTHER
      val stats = await(bookRepository.getInteractionStats(bookId))
      val interactions = await(bookRepository.getUserInteractions(bookId, likerId))

      stats._1 mustBe 1
      interactions._1 mustBe true
    }

    "allow an authenticated user to bookmark a book" in {
      val bookRepository = app.injector.instanceOf[BookRepository]
      val ownerId = createUser("bookmark-owner@example.com", "Bookmark Owner")
      val bookmarkerId = createUser("bookmarker@example.com", "Bookmarker")
      val bookId = await(
        bookRepository.insert(
          Book(
            ownerId = ownerId,
            title = "Bookmarkable Book",
            author = "Author",
            description = Some("Bookmark test")
          )
        )
      )

      val request = addCSRFToken(
        FakeRequest(POST, s"/books/$bookId/bookmark")
          .withSession("userEmail" -> "bookmarker@example.com")
      )
      val result = route(app, request).value
      status(result) mustBe SEE_OTHER
      val stats = await(bookRepository.getInteractionStats(bookId))
      val interactions = await(bookRepository.getUserInteractions(bookId, bookmarkerId))

      stats._3 mustBe 1
      interactions._3 mustBe true
    }

    "allow an authenticated user to comment on a book" in {
      val bookRepository = app.injector.instanceOf[BookRepository]
      val ownerId = createUser("comment-owner@example.com", "Comment Owner")
      val commenterId = createUser("commenter@example.com", "Commenter")
      val bookId = await(
        bookRepository.insert(
          Book(
            ownerId = ownerId,
            title = "Commentable Book",
            author = "Author",
            description = Some("Comment test")
          )
        )
      )

      val request = addCSRFToken(
        FakeRequest(POST, s"/books/$bookId/comment")
          .withSession("userEmail" -> "commenter@example.com")
          .withFormUrlEncodedBody("content" -> "Great catalog entry")
      )
      val result = route(app, request).value
      status(result) mustBe SEE_OTHER
      val comments = await(bookRepository.listComments(bookId))
      val stats = await(bookRepository.getInteractionStats(bookId))

      comments.size mustBe 1
      comments.head._1.content mustBe "Great catalog entry"
      comments.head._1.userId mustBe commenterId
      stats._4 mustBe 1
    }

    "allow an authenticated user to repost a book" in {
      val bookRepository = app.injector.instanceOf[BookRepository]
      val ownerId = createUser("repost-owner@example.com", "Repost Owner")
      val reposterId = createUser("reposter@example.com", "Reposter")
      val bookId = await(
        bookRepository.insert(
          Book(
            ownerId = ownerId,
            title = "Repostable Book",
            author = "Author",
            description = Some("Repost test")
          )
        )
      )

      val request = addCSRFToken(
        FakeRequest(POST, s"/books/$bookId/repost")
          .withSession("userEmail" -> "reposter@example.com")
      )
      val result = route(app, request).value
      status(result) mustBe SEE_OTHER
      val stats = await(bookRepository.getInteractionStats(bookId))
      val interactions = await(bookRepository.getUserInteractions(bookId, reposterId))

      stats._2 mustBe 1
      interactions._2 mustBe true
    }

    "allow an authenticated user to reply to a comment" in {
      val bookRepository = app.injector.instanceOf[BookRepository]
      val ownerId = createUser("reply-owner@example.com", "Reply Owner")
      val commenterId = createUser("root-commenter@example.com", "Root Commenter")
      val replierId = createUser("replier@example.com", "Replier")
      val bookId = await(
        bookRepository.insert(
          Book(
            ownerId = ownerId,
            title = "Replyable Book",
            author = "Author",
            description = Some("Reply test")
          )
        )
      )
      val parentCommentId = await(bookRepository.addComment(commenterId, bookId, "Root comment"))

      val request = addCSRFToken(
        FakeRequest(POST, s"/books/$bookId/comment")
          .withSession("userEmail" -> "replier@example.com")
          .withFormUrlEncodedBody(
            "content" -> "Nested reply",
            "parentId" -> parentCommentId.toString
          )
      )
      val result = route(app, request).value
      status(result) mustBe SEE_OTHER
      val comments = await(bookRepository.listComments(bookId))
      val reply = comments.find(_._1.parentId.contains(parentCommentId)).value._1

      reply.content mustBe "Nested reply"
      reply.userId mustBe replierId
    }

    "render the discover page with recommended users" in {
      createUser("discover-a@example.com", "Discover A")
      createUser("discover-b@example.com", "Discover B")

      val request = FakeRequest(GET, "/users/discover")
      val result = route(app, request).value

      status(result) mustBe OK
      contentAsString(result) must include ("Discover Readers")
      contentAsString(result) must include ("Discover A")
    }

    "allow an owner to checkout an available item" in {
      val bookRepository = app.injector.instanceOf[BookRepository]
      val ownerId = createUser("owner-loan@example.com", "Loan Owner")
      val borrowerId = createUser("borrower-loan@example.com", "Loan Borrower")
      createPolicy(ownerId, defaultLoanDays = 10)
      val (_, itemId) = createBookWithItem(ownerId, "Checkout Book")

      val request = addCSRFToken(
        FakeRequest(POST, "/loans/checkout")
          .withSession("userEmail" -> "owner-loan@example.com")
          .withFormUrlEncodedBody(
            "bookItemId" -> itemId.toString,
            "borrowerEmail" -> "borrower-loan@example.com"
          )
      )
      val result = route(app, request).value
      status(result) mustBe SEE_OTHER
      val loans = await(bookRepository.listLoansByOwner(ownerId))
      val item = await(bookRepository.findItemById(itemId)).value

      loans.size mustBe 1
      loans.head.borrowerId mustBe borrowerId
      loans.head.dueAt mustBe LocalDate.now().plusDays(10)
      item.status mustBe "LOANED"
    }

    "allow an owner to extend an active loan within policy limits" in {
      val bookRepository = app.injector.instanceOf[BookRepository]
      val ownerId = createUser("owner-extend@example.com", "Extend Owner")
      val borrowerId = createUser("borrower-extend@example.com", "Extend Borrower")
      createPolicy(ownerId, defaultLoanDays = 5, maxExtensions = 2)
      val (_, itemId) = createBookWithItem(ownerId, "Extend Book")
      val loanId = await(
        bookRepository.insertLoan(
          Loan(
            ownerId = ownerId,
            borrowerId = borrowerId,
            bookItemId = itemId,
            loanedAt = LocalDate.now(),
            dueAt = LocalDate.now().plusDays(5),
            returnedAt = None,
            extensionsUsed = 0,
            status = "ACTIVE"
          )
        )
      )
      await(bookRepository.updateItemStatus(itemId, "LOANED"))

      val request = addCSRFToken(
        FakeRequest(POST, s"/loans/$loanId/extend")
          .withSession("userEmail" -> "owner-extend@example.com")
      )
      val result = route(app, request).value
      status(result) mustBe SEE_OTHER
      val loan = await(bookRepository.findLoanById(loanId)).value

      loan.extensionsUsed mustBe 1
      loan.dueAt mustBe LocalDate.now().plusDays(10)
    }

    "allow an owner to return a loan and restore item availability" in {
      val bookRepository = app.injector.instanceOf[BookRepository]
      val ownerId = createUser("owner-return@example.com", "Return Owner")
      val borrowerId = createUser("borrower-return@example.com", "Return Borrower")
      createPolicy(ownerId)
      val (_, itemId) = createBookWithItem(ownerId, "Return Book")
      val loanId = await(
        bookRepository.insertLoan(
          Loan(
            ownerId = ownerId,
            borrowerId = borrowerId,
            bookItemId = itemId,
            loanedAt = LocalDate.now(),
            dueAt = LocalDate.now().plusDays(7),
            returnedAt = None,
            extensionsUsed = 0,
            status = "ACTIVE"
          )
        )
      )
      await(bookRepository.updateItemStatus(itemId, "LOANED"))

      val request = addCSRFToken(
        FakeRequest(POST, s"/loans/$loanId/return")
          .withSession("userEmail" -> "owner-return@example.com")
      )
      val result = route(app, request).value
      status(result) mustBe SEE_OTHER
      val loan = await(bookRepository.findLoanById(loanId)).value
      val item = await(bookRepository.findItemById(itemId)).value

      loan.status mustBe "RETURNED"
      loan.returnedAt mustBe Some(LocalDate.now())
      item.status mustBe "AVAILABLE"
    }

    "allow a borrower to settle an overdue fee" in {
      val bookRepository = app.injector.instanceOf[BookRepository]
      val ownerId = createUser("owner-settle@example.com", "Settle Owner")
      val borrowerId = createUser("borrower-settle@example.com", "Settle Borrower")
      createPolicy(ownerId, dailyOverdueFee = BigDecimal(1.25))
      val (_, itemId) = createBookWithItem(ownerId, "Overdue Book")
      val dueAt = LocalDate.now().minusDays(4)
      val returnedAt = LocalDate.now()
      val loanId = await(
        bookRepository.insertLoan(
          Loan(
            ownerId = ownerId,
            borrowerId = borrowerId,
            bookItemId = itemId,
            loanedAt = LocalDate.now().minusDays(10),
            dueAt = dueAt,
            returnedAt = Some(returnedAt),
            extensionsUsed = 0,
            status = "RETURNED"
          )
        )
      )

      val request = addCSRFToken(
        FakeRequest(POST, s"/loans/$loanId/settle-fee")
          .withSession("userEmail" -> "borrower-settle@example.com")
      )
      val result = route(app, request).value
      status(result) mustBe SEE_OTHER
      val loan = await(bookRepository.findLoanById(loanId)).value
      val payments = await(bookRepository.listLoanPaymentsByLoanIds(Seq(loanId))).getOrElse(loanId, Nil)

      loan.overdueFeePaidAt mustBe Some(LocalDate.now())
      loan.overdueFeePaidAmount mustBe BigDecimal(5.00)
      loan.accruedOverdueFee mustBe BigDecimal(5.00)
      payments must have size 1
      payments.head.amount mustBe BigDecimal(5.00)
      payments.head.receiptNumber must startWith ("RCPT-")
    }

    "allow a borrower to make a partial overdue fee payment" in {
      val bookRepository = app.injector.instanceOf[BookRepository]
      val ownerId = createUser("owner-partial@example.com", "Partial Owner")
      val borrowerId = createUser("borrower-partial@example.com", "Partial Borrower")
      createPolicy(ownerId, dailyOverdueFee = BigDecimal(1.25))
      val (_, itemId) = createBookWithItem(ownerId, "Partial Fee Book")
      val loanId = await(
        bookRepository.insertLoan(
          Loan(
            ownerId = ownerId,
            borrowerId = borrowerId,
            bookItemId = itemId,
            loanedAt = LocalDate.now().minusDays(10),
            dueAt = LocalDate.now().minusDays(4),
            returnedAt = Some(LocalDate.now()),
            extensionsUsed = 0,
            status = "RETURNED"
          )
        )
      )

      val request = addCSRFToken(
        FakeRequest(POST, s"/loans/$loanId/pay-fee")
          .withSession("userEmail" -> "borrower-partial@example.com")
          .withFormUrlEncodedBody("amount" -> "2.50")
      )
      val result = route(app, request).value
      status(result) mustBe SEE_OTHER
      val loan = await(bookRepository.findLoanById(loanId)).value
      val payments = await(bookRepository.listLoanPaymentsByLoanIds(Seq(loanId))).getOrElse(loanId, Nil)

      loan.overdueFeePaidAmount mustBe BigDecimal(2.50)
      loan.accruedOverdueFee mustBe BigDecimal(5.00)
      loan.overdueFeePaidAt mustBe None
      payments must have size 1
      payments.head.amount mustBe BigDecimal(2.50)
      payments.head.receiptNumber must startWith ("RCPT-")
    }

    "allow an authorized user to download a receipt" in {
      val bookRepository = app.injector.instanceOf[BookRepository]
      val ownerId = createUser("owner-receipt@example.com", "Receipt Owner")
      val borrowerId = createUser("borrower-receipt@example.com", "Receipt Borrower")
      createPolicy(ownerId, dailyOverdueFee = BigDecimal(1.00))
      val (_, itemId) = createBookWithItem(ownerId, "Receipt Book")
      val loanId = await(
        bookRepository.insertLoan(
          Loan(
            ownerId = ownerId,
            borrowerId = borrowerId,
            bookItemId = itemId,
            loanedAt = LocalDate.now().minusDays(10),
            dueAt = LocalDate.now().minusDays(3),
            returnedAt = Some(LocalDate.now()),
            extensionsUsed = 0,
            status = "RETURNED"
          )
        )
      )
      await(bookRepository.payOverdueFee(loanId, BigDecimal(2.00), LocalDate.now()))
      val receipt = await(bookRepository.listLoanPaymentsByLoanIds(Seq(loanId))).getOrElse(loanId, Nil).head

      val request = FakeRequest(GET, s"/receipts/${receipt.receiptNumber}")
        .withSession("userEmail" -> "borrower-receipt@example.com")
      val result = route(app, request).value

      status(result) mustBe OK
      contentAsString(result) must include ("BookNetwork Receipt")
      contentAsString(result) must include (receipt.receiptNumber)
      headers(result).get("Content-Disposition").value must include (s"""filename="${receipt.receiptNumber}.txt"""")
    }

    "allow an authenticated user to save an ISBN-linked reading post" in {
      val bookRepository = app.injector.instanceOf[BookRepository]
      createUser("reader-post@example.com", "Reader Post")

      val request = addCSRFToken(
        FakeRequest(POST, "/books/save")
          .withSession("userEmail" -> "reader-post@example.com")
          .withFormUrlEncodedBody(
            "title" -> "Deep Work",
            "author" -> "Cal Newport",
            "reviewHeadline" -> "A sharp push against distraction",
            "readingStatus" -> "FINISHED",
            "rating" -> "5",
            "description" -> "This made me rethink how I protect attention.",
            "metadata.isbn13" -> "978-1-4555-8669-1",
            "metadata.publisher" -> "Grand Central Publishing"
          )
      )
      val result = route(app, request).value
      status(result) mustBe SEE_OTHER

      val isbnPosts = await(bookRepository.listByIsbn("9781455586691"))
      isbnPosts must have size 1
      isbnPosts.head.reviewHeadline mustBe Some("A sharp push against distraction")
      isbnPosts.head.readingStatus mustBe "FINISHED"
      isbnPosts.head.rating mustBe Some(5)
      isbnPosts.head.metadata.isbn13 mustBe Some("9781455586691")
    }

    "render the isbn community page for shared reader posts" in {
      val bookRepository = app.injector.instanceOf[BookRepository]
      val ownerA = createUser("isbn-a@example.com", "ISBN A")
      val ownerB = createUser("isbn-b@example.com", "ISBN B")
      await(
        bookRepository.insert(
          Book(
            ownerId = ownerA,
            title = "Atomic Habits",
            author = "James Clear",
            description = Some("Great habit design examples."),
            reviewHeadline = Some("Practical systems over goals"),
            readingStatus = "FINISHED",
            rating = Some(5),
            metadata = models.BookMetadata(isbn13 = Some("9780735211292"))
          )
        )
      )
      await(
        bookRepository.insert(
          Book(
            ownerId = ownerB,
            title = "Atomic Habits",
            author = "James Clear",
            description = Some("Useful for forming routines."),
            reviewHeadline = Some("Easy to apply immediately"),
            readingStatus = "READING",
            rating = Some(4),
            metadata = models.BookMetadata(isbn13 = Some("9780735211292"))
          )
        )
      )

      val request = FakeRequest(GET, "/books/isbn/9780735211292")
      val result = route(app, request).value

      status(result) mustBe OK
      contentAsString(result) must include ("ISBN Community")
      contentAsString(result) must include ("Practical systems over goals")
      contentAsString(result) must include ("Easy to apply immediately")
    }

    "forbid receipt download for unrelated users" in {
      val bookRepository = app.injector.instanceOf[BookRepository]
      val ownerId = createUser("owner-receipt-block@example.com", "Receipt Block Owner")
      val borrowerId = createUser("borrower-receipt-block@example.com", "Receipt Block Borrower")
      createUser("outsider-receipt@example.com", "Receipt Outsider")
      createPolicy(ownerId, dailyOverdueFee = BigDecimal(1.00))
      val (_, itemId) = createBookWithItem(ownerId, "Receipt Guard Book")
      val loanId = await(
        bookRepository.insertLoan(
          Loan(
            ownerId = ownerId,
            borrowerId = borrowerId,
            bookItemId = itemId,
            loanedAt = LocalDate.now().minusDays(8),
            dueAt = LocalDate.now().minusDays(2),
            returnedAt = Some(LocalDate.now()),
            extensionsUsed = 0,
            status = "RETURNED"
          )
        )
      )
      await(bookRepository.payOverdueFee(loanId, BigDecimal(1.00), LocalDate.now()))
      val receipt = await(bookRepository.listLoanPaymentsByLoanIds(Seq(loanId))).getOrElse(loanId, Nil).head

      val request = FakeRequest(GET, s"/receipts/${receipt.receiptNumber}")
        .withSession("userEmail" -> "outsider-receipt@example.com")
      val result = route(app, request).value

      status(result) mustBe FORBIDDEN
    }
  }
}
