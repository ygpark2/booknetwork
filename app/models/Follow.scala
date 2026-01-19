package models

import slick.jdbc.H2Profile.api._
import play.api.db.slick._
import java.util.Date
import java.sql.{ Date => SqlDate }
import slick.lifted.Tag
import views.html.helper._
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Lang

case class Follow(id: Option[Int] = None, followerId: Int, followeeId: Int, createdAt: java.time.LocalDateTime) extends Entity

abstract class FollowsTable(tag: Tag) extends Table[Follow](tag, "FOLLOW") with TableBase[Follow] {
  def * = (id.?, followerId, followeeId, createdAt) <> (Follow.tupled, Follow.unapply)
  def ? = (id.?, followerId.?, followeeId.?, createdAt.?).shaped.<>({r=>import r._; _1.map(_=> Follow.tupled((_1, _2.get, _3.get, _4.get)))}, (_:Any) =>
    throw new Exception("Inserting into ? projection not supported."))

  val followerId: Rep[Int] = column[Int]("FOLLOWER_ID")
  val followeeId: Rep[Int] = column[Int]("FOLLOWEE_ID")
  val createdAt: Rep[java.time.LocalDateTime] = column[java.time.LocalDateTime]("CREATED_AT", O.Default(java.time.LocalDateTime.now()))

  val id: Column[Int] = column[Int]("ID", O.AutoInc, O.PrimaryKey)

  def tinyDescription = s"Follow ${followerId} -> ${followeeId}"

}

class Follows(tag: Tag) extends FollowsTable(tag)

class FollowModel extends Model[Follow]{
  val playForm = Form(
    mapping(
      "id" -> optional(number),
      "followerId" -> number,
      "followeeId" -> number,
      "createdAt" -> localDateTime
    )(Follow.apply)(Follow.unapply)
  )
  def form(playForm: Form[Follow]) = FollowForm(playForm=playForm)

  class FollowLabels extends super.Labels {
    def singular = "Follow".toLowerCase
    def plural   = "Follows".toLowerCase
    object columns {
      def id: String = "Id"
      def followerId: String = "Follower ID"
      def followeeId: String = "Followee ID"
      def createdAt: String = "Created At"
    }
  }

  val labels = new FollowLabels

  val referencedModels: Map[String,Model[_ <: Entity]] = Map(
    "followerId" -> Users,
    "followeeId" -> Users
  )

  def referencedModelsAndIds(entities: Seq[Follow])(implicit db: JdbcBackend#Database): Future[Map[Model[_ <: Entity],Map[Int,Option[(Int,String)]]]] = {
    Future.successful(Map(
      Users -> entities.flatMap(e => Seq(
        e.id.get -> Some((e.followerId, "")),
        e.id.get -> Some((e.followeeId, ""))
      )).toMap
    ))
  }

  override def tinyDescription(e: Follow) = s"Follow ${e.followerId} -> ${e.followeeId}"

  val schema = Map(
    "followerId" -> ("Int", false),
    "followeeId" -> ("Int", false),
    "createdAt" -> ("LocalDateTime", false)
  )

  final val query = TableQuery[Follows]

  override val html = new HtmlForm

  class HtmlForm extends Html{
    def headings = Seq(labels.columns.followerId, labels.columns.followeeId)
    def cells(e: Follow) = {
      def render(v: Any) = v match {
        case None => <em> - </em>
        case d:java.sql.Date => new java.text.SimpleDateFormat("dd MMM yyyy").format(d)
        case d:java.time.LocalDateTime => d.toString
        case v => v.toString
      }
      Seq[Any](e.followerId, e.followeeId).map{
        case Some(v) => render(v)
        case v => render(v)
      }
    }
  }
}

object Follows extends FollowModel

case class FollowForm(playForm: Form[Follow]) extends ModelForm[Follow]{
  val model = Follows
  override val html = new HtmlForm
  class HtmlForm extends Html{
    def allInputs(implicit handler: FieldConstructor, lang: Lang) = Seq(
      inputs.followerId,
      inputs.followeeId
    )
    object inputs{
      def followerId(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("followerId"), Symbol("_label") -> model.labels.columns.followerId)
      def followeeId(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("followeeId"), Symbol("_label") -> model.labels.columns.followeeId)
      def id(implicit handler: FieldConstructor, lang: Lang) = inputText(playForm("id"), Symbol("_label") -> model.labels.columns.id)
    }
  }
}

object follows extends TableQuery(tag => new Follows(tag))