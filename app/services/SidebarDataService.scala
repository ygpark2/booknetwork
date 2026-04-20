package services

import javax.inject._
import models.RecommendedUser
import repositories.{BookRepository, UserRepository}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SidebarDataService @Inject()(bookRepository: BookRepository, userRepository: UserRepository)(implicit ec: ExecutionContext) {

  def trendingBooks(limit: Int = 3): Future[Seq[models.Book]] =
    bookRepository.trending(limit)

  def recommendedUsers(emailOpt: Option[String], limit: Int = 3): Future[Seq[RecommendedUser]] = {
    emailOpt match {
      case Some(email) =>
        userRepository.findByEmail(email).flatMap {
          case Some(currentUser) =>
            userRepository.listRecommendUsers(Some(currentUser.id), limit).flatMap { users =>
              Future.sequence(users.map { user =>
                userRepository.isFollowing(currentUser.id, user.id).map(isFollowing => RecommendedUser(user, isFollowing))
              })
            }
          case None =>
            userRepository.listRecommendUsers(None, limit).map(_.map(user => RecommendedUser(user, isFollowing = false)))
        }
      case None =>
        userRepository.listRecommendUsers(None, limit).map(_.map(user => RecommendedUser(user, isFollowing = false)))
    }
  }
}
