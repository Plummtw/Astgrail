package org.plummtw.astgrail.model

import net.liftweb._
import net.liftweb.mapper._
import net.liftweb.util.FieldError

import scala.util.matching.Regex
//import http._ 
//import SHtml._
//import util._
//import java.util.Date

import org.plummtw.astgrail.enum._
import org.plummtw.astgrail.lib._
import org.plummtw.astgrail.util.PlummUtil

class UserIcon extends LongKeyedMapper[UserIcon] with CreatedUpdated with IdPK {
  def getSingleton = UserIcon

  object user_id       extends MappedLongForeignKey(this, User)

  object icon_group    extends MappedInt(this)
  object icon_gname    extends MappedString(this, 20) {
    override def validations = validPriority _ :: super.validations

    def validPriority(in: String): List[FieldError] =
      List(if (in.length() > 20)         List(FieldError(this, <b>gname過長＞２０</b>)) else Nil,
           if (PlummUtil.hasHtmlCode(in)) List(FieldError(this, <b>gname包含控制碼</b>)) else Nil).flatten
  }
  
  object icon_name     extends MappedString(this, 20) {
    override def validations = validPriority _ :: super.validations

    def validPriority(in: String): List[FieldError] =
      List(if (in.length() <  1)          List(FieldError(this, <b>名稱過短＜１</b>))
           else if (in.length() > 20)     List(FieldError(this, <b>名稱過長＞２０</b>))
           else Nil,
           if (PlummUtil.hasHtmlCode(in)) List(FieldError(this, <b>名稱包含控制碼</b>)) else Nil).flatten
  }
  
  object icon_filename extends MappedString(this, 80) {
    override def validations = validPriority _ :: super.validations

    def validPriority(in: String): List[FieldError] =
      List(if (in.length() <  1)         List(FieldError(this, <b>檔名過短＜１</b>))
             else if (in.length() > 80)  List(FieldError(this, <b>檔名過長＞８０</b>))
             else Nil,
           if (PlummUtil.hasHtmlCode(in)) List(FieldError(this, <b>檔名包含控制碼</b>)) else Nil).flatten
  }
  
  
  object icon_width    extends MappedInt(this)
  object icon_height   extends MappedInt(this)
  object color         extends MappedString(this,7)  {
    override def validations = validPriority _ :: super.validations

    val regex = new Regex("#[0-9A-f]{6}")
    def validPriority(in: String) = in match {
      case regex() => Nil
      case _      => List(FieldError(this, "顏色格式錯誤"))
    }
  }
}

object UserIcon extends UserIcon with LongKeyedMetaMapper[UserIcon] {
  override def fieldOrder = List(id, user_id, icon_group, icon_gname, icon_name, icon_filename,
                                 icon_width, icon_height, color)
  
  def create_default_usericon() {
    val default_icons = List(
      //List("替身君", "dummy_boy_user_icon.gif", "#000000"),
      List("明灰",   "001.gif", "#DDDDDD"),
      List("暗灰",   "002.gif", "#999999"),
      List("黃色",   "003.gif", "#FFD700"),
      List("橘色",   "004.gif", "#FF9900"),
      List("紅色",   "005.gif", "#FF0000"),
      List("水色",   "006.gif", "#99CCFF"),
      List("青",     "007.gif", "#0066FF"),
      List("綠",     "008.gif", "#00EE00"),
      List("紫",     "009.gif", "#CC00CC"),
      List("櫻花色", "010.gif", "#FF9999"))

    default_icons.foreach { default_icon =>
      val user_icon = UserIcon.create.icon_group(0).icon_gname("")
                              .icon_name(default_icon(0)).icon_filename("user_icons/" + default_icon(1))
                              .icon_width(45).icon_height(45).color(default_icon(2))
      user_icon.save
    }
  }
}

object UserIconCache extends TimedCache[Long, UserIcon](30000)