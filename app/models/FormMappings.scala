package models

import play.api.data.Mapping
import play.api.data.Forms._
import play.api.data.format._
import play.api.data.format.Formats._
import java.sql.Timestamp

object FormMappings {

  implicit val sqlTimestampFormat: Formatter[java.sql.Timestamp] = sqlTimestampFormat("yyyy-MM-dd HH:mm")

  implicit val localDateTimeFormat: Formatter[java.time.LocalDateTime] = new Formatter[java.time.LocalDateTime] {
    override val format = Some("format.datetime", Seq("yyyy-MM-dd'T'HH:mm"))

    def bind(key: String, data: Map[String, String]) = {
      data.get(key).map { value =>
        try {
          Right(java.time.LocalDateTime.parse(value))
        } catch {
          case _: Exception => Left(Seq(play.api.data.validation.ValidationError("error.datetime", "yyyy-MM-dd'T'HH:mm")))
        }
      }.getOrElse(Left(Seq(play.api.data.validation.ValidationError("error.required"))))
    }

    def unbind(key: String, value: java.time.LocalDateTime) = Map(key -> value.toString)
  }

  def sqlTimestampFormat(pattern: String): Formatter[java.sql.Timestamp] = new Formatter[java.sql.Timestamp] {
    override val format = Some("format.date", Seq(pattern))

    def bind(key: String, data: Map[String, String]) = {
      dateFormat(pattern).bind(key, data).right.map(d => new java.sql.Timestamp(d.getTime))
    }

    def unbind(key: String, value: java.sql.Timestamp) = dateFormat(pattern).unbind(key, value)
  }

  def sqlTimestamp(pattern: String): Mapping[java.sql.Timestamp] = of[java.sql.Timestamp] as sqlTimestampFormat(pattern)

  def localDateTime: Mapping[java.time.LocalDateTime] = of[java.time.LocalDateTime] as localDateTimeFormat

}