package models

import java.time.LocalDate
import java.time.Instant
import slick.jdbc.H2Profile.api._
import slick.jdbc.JdbcBackend
import javax.inject.Inject
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.data.Form
import views.html.helper._
import play.api.i18n.Lang

object Model{
  def all = byName.values
  def byName: Map[String,Model[_ <: Entity,_]] = Map(
    "user" -> Users,
    "book" -> Books,
    "loan" -> Loans,
    "post" -> Posts,
    "comment" -> Comments,
    "like" -> Likes,
    "follow" -> Follows,
    "review" -> Reviews
  )
}

trait Entity{
  def id: Option[Int]
}

abstract class Model[E <: Entity] {
  def query: slick.jdbc.H2Profile.api.TableQuery[Table[E]]

  def referencedModels: Map[String,Model[_ <: Entity]]

  // GUI related stuff
  trait Labels{
    def singular: String
    def plural: String
  }
  def labels: Labels

  def tinyDescription(e: E): String = labels.singular.capitalize + s"(${e.id})"

  /** model -> map(entity id -> option(related entity id -> entity tiny description))*/
  def referencedModelsAndIds(entities: Seq[E])(implicit db: JdbcBackend#Database): Future[Map[Model[_ <: Entity],Map[Int,Option[(Int,String)]]]]

  trait Html{
    def headings: Seq[String]
    def cells(e: E): Seq[java.io.Serializable]
  }
  def html: Html

  // FORM stuff
  def form(playForm: Form[E]): ModelForm[E]
  def playForm: Form[E]
  /** column name -> (type, required) */
  def schema: Map[String,(String,Boolean)]

  // CRUD

  def findById(id: Int)(implicit db: JdbcBackend#Database): Future[Option[E]] = db.run(query.filter(_.id === id).result.headOption)

  def update(entity: E)(implicit db: JdbcBackend#Database): Future[Int] = entity.id match {
    case Some(id) => db.run(query.filter(_.id === id).update(entity))
    case None => throw new Exception("cannot update entity without id")
  }

  def delete(id: Int)(implicit db: JdbcBackend#Database): Future[Int] = db.run(query.filter(_.id === id).delete)

  def insert(entity: E)(implicit db: JdbcBackend#Database): Future[Int] = db.run(query += entity)

  // OTHER

  def typed[R](body: Model[E] => R) = body(this)

  // USE CASES

  def options()(implicit db: JdbcBackend#Database): Future[Seq[(String, String)]] = {
    db.run(query.map(r => r.id.asColumnOf[String] -> r.tinyDescription).sortBy(_._2).result)
  }

  def count()(implicit db: JdbcBackend#Database): Future[Int] = db.run(query.length.result)

  def count(filter: String)(implicit db: JdbcBackend#Database): Future[Int] = {
    db.run(query.filter(_.tinyDescription.toLowerCase like s"%${filter.toLowerCase}%").length.result)
  }

  def list(page: Int = 0, pageSize: Int = 10, orderBy: Int = 1, filter: String = "%")(implicit db: JdbcBackend#Database): Future[Page[E]] = {
    val offset = pageSize * page
    val queryFiltered = query.filter(_.tinyDescription.toLowerCase like s"%${filter.toLowerCase}%")

    for {
      total <- count(filter)
      items <- db.run(queryFiltered.drop(offset).take(pageSize).result)
    } yield Page(items, page, offset, total)
  }
}

trait ModelForm[E <: Entity]{
  def playForm: Form[E]
  def model: Model[E]
  trait Html{
    def allInputs(implicit handler: FieldConstructor): Seq[play.twirl.api.HtmlFormat.Appendable]
  }
  def html: Html
}

case class Page[A](items: Seq[A], page: Int, offset: Long, total: Long) {
  lazy val prev = Option(page - 1).filter(_ >= 0)
  lazy val next = Option(page + 1).filter(_ => (offset + items.size) < total)
}

trait TableBase[E] extends slick.jdbc.H2Profile.api.Table[E]{
  def id: Rep[Int]
  def tinyDescription: Rep[String]
}
