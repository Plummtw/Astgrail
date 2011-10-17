package org.plummtw.astgrail.model

import net.liftweb.mapper._
import net.liftweb.util.FieldError
import net.liftweb.http.{SHtml, SessionVar}

import net.liftweb.common.{Empty, Box, Full}

import scala.util.matching._
import scala.collection.immutable.HashMap

import org.plummtw.astgrail.enum._
import org.plummtw.astgrail.util.PlummUtil

object CurrentUser extends SessionVar[Box[User]](Empty)

class User extends LongKeyedMapper[User] with CreatedUpdated with IdPK {
  //override def dbName = "User"
  def getSingleton = User
  val email_regex = new Regex(".+@.+\\.[a-z]+")

  // Login 用 id
  object uname         extends MappedString(this, 20) {
    override def validations = validPriority _ :: super.validations

    def validPriority(in: String): List[FieldError] =
      List(if (in.length() <  6)           List(FieldError(this, <b>帳號過短＜６</b>))
             else if (in.length() > 20)  List(FieldError(this, <b>帳號過長＞２０</b>))
             else Nil,
           if (PlummUtil.hasHtmlCode(in)) List(FieldError(this, <b>帳號包含控制碼</b>)) else Nil).flatten
  }

  // 顯示用暱名
  object handle_name   extends MappedString(this, 20) {
    override def validations = validPriority _ :: super.validations

    def validPriority(in: String): List[FieldError] =
      List(if (in.length() <  1)           List(FieldError(this, <b>暱稱過短＜１</b>))
             else if (in.length() > 20)  List(FieldError(this, <b>暱稱過長＞２０</b>))
             else Nil,
           if (PlummUtil.hasHtmlCode(in)) List(FieldError(this, <b>暱稱包含控制碼</b>)) else Nil).flatten
  }

  object trip   extends MappedString(this, 20) {
    override def validations = validPriority _ :: super.validations

    def validPriority(in: String): List[FieldError] =
      List(if (in.length() <  6)         List(FieldError(this, <b>ｔｒｉｐ過短＜６</b>))
             else if (in.length() > 20)  List(FieldError(this, <b>ｔｒｉｐ過長＞２０</b>))
             else Nil,
           if (PlummUtil.hasHtmlCode(in)) List(FieldError(this, <b>ｔｒｉｐ包含控制碼</b>)) else Nil).flatten
  }

  object password      extends MappedString(this, 20) {
    override def validations = validPriority _ :: super.validations

    def validPriority(in: String): List[FieldError] =
      List(if (in.length() < 6)          List(FieldError(this, <b>密碼過短＜６</b>))
             else if (in.length() > 20)  List(FieldError(this, <b>密碼過長＞２０</b>))
             else Nil,
           if (PlummUtil.hasHtmlCode(in)) List(FieldError(this, <b>密碼包含控制碼</b>)) else Nil).flatten
  }

  object sex           extends MappedString(this, 1)  {
    override def validations = validPriority _ :: super.validations
    override def defaultValue = "M"

    val sex_map: Map[String, String] = Map("M"->"男", "F"->"女")
    def sex_radios =
      SHtml.radio(sex_map.keys.toList, Full(User.this.sex.toString), User.this.sex(_))

    def generateHtml = sex_radios.flatMap(PlummUtil.htmlize(_, sex_map))

    def validPriority(in: String): List[FieldError] =
      if (!sex_map.contains(in)) List(FieldError(this, <b>性別錯誤</b>))
      else Nil
  }

  object email      extends MappedString(this,80) {
    override def validations = validPriority _ :: super.validations

    def validPriority(in: String): List[FieldError] =
      List(if (email_regex.findFirstIn(in).isEmpty) List(FieldError(this, <b>EMail格式不合</b>)) else Nil,
           if (in.length() > 80) List(FieldError(this, <b>EMail過長＞８０</b>)) else Nil,
           if (PlummUtil.hasHtmlCode(in)) List(FieldError(this, <b>EMail包含控制碼</b>)) else Nil).flatten
  }

   object msn      extends MappedString(this,80) {
    override def validations = validPriority _ :: super.validations

    def validPriority(in: String): List[FieldError] =
      List(if (email_regex.findFirstIn(in).isEmpty) List(FieldError(this, <b>Msn格式不合</b>)) else Nil,
           if (in.length() > 80)  List(FieldError(this, <b>Msn過長＞８０</b>)) else Nil,
           if (PlummUtil.hasHtmlCode(in)) List(FieldError(this, <b>Msn包含控制碼</b>)) else Nil).flatten
  }

  object zodiac    extends MappedString(this,3) {
    override def validations = validPriority _ :: super.validations
    override def defaultValue = "Ari"

    val zodiac_map  = HashMap("Ari" -> "牡羊座", "Tau" -> "金牛座", "Gem" -> "雙子座", "Can" -> "巨蟹座",
                            "Leo" -> "獅子座", "Vir" -> "處女座", "Lib" -> "天秤座", "Sco" -> "天蠍座",
                            "Sag" -> "射手座", "Cap" -> "魔羯座", "Aqu" -> "水瓶座", "Pis" -> "雙魚座")
    val zodiac_list = List("Ari", "Tau", "Gem", "Can", "Leo", "Vir", "Lib", "Sco", "Sag", "Cap", "Aqu", "Pis")

    val zodiac_seq_pair = zodiac_list.map(x => (x,zodiac_map.get(x).getOrElse("")))

    def generateHtml = SHtml.select(zodiac_seq_pair, Full(User.this.zodiac), User.this.zodiac(_))

    def validPriority(in: String): List[FieldError] =
      if (!zodiac_map.contains(in)) List(FieldError(this, <b>星座錯誤</b>))
      else Nil
  }

  // 喜好的圖像
  object user_icon_id  extends MappedLongForeignKey(this, UserIcon) {
    // override def validations = validPriority _ :: super.validations

    //def validPriority(in: Long): List[FieldError] =
    //  if (in == 0)  List(FieldError(this, <b>尚未選擇圖像</b>))
    //  else Nil
  }

  object user_memo  extends MappedString(this,3000) {
    override def defaultValue = ""

    def validPriority(in: String): List[FieldError] =
      if (in.length() > 80)          List(FieldError(this, <b>備註過長＞３０００</b>))
      else Nil
  }

  object user_score  extends MappedInt(this) {
    override def defaultValue = 0
  }

  object user_flags  extends MappedEnumList(this,UserFlagEnum) {
    override def defaultValue = Seq()
    
  }

  object created_ip  extends MappedString(this, 20) {
    override def defaultValue = PlummUtil.getIpAddress()
  }

  object last_login    extends MappedDateTime(this) {
    override def defaultValue = new java.util.Date()
  }
}

object User extends User with LongKeyedMetaMapper[User] {
  override def fieldOrder = List(id, uname, handle_name, trip, password, sex, email, msn,
                                   zodiac, user_icon_id,
                                   user_score, user_flags, created_ip)

  def logined_? = CurrentUser.is match {
    case Full(user)  => true
    case _           => false
  }
}

object User_System extends User {
  handle_name("系統")
}