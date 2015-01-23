package models

import scala.slick.lifted.MappedTypeMapper.base
import scala.slick.lifted.MappedTypeMapper

sealed trait Permission
case object Administrator extends Permission
case object NormalUser extends Permission

object Permission {
  implicit val PermissionTimeMapper = MappedTypeMapper.base[Permission, String](
    d => Permission.stringValueOf(d),
    t => Permission.valueOf(t))

  def valueOf(value: String): Permission = value match {
    case "Administrator" => Administrator
    case "NormalUser" => NormalUser
    case _ => throw new IllegalArgumentException()
  }

  def stringValueOf(value: Permission): String = value match {
    case Administrator => "Administrator" 
    case NormalUser => "NormalUser" 
    case _ => throw new IllegalArgumentException()
  }
}
