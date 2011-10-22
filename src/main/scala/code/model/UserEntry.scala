package org.plummtw.astgrail.model

import net.liftweb._
import net.liftweb.mapper._
import net.liftweb.util.FieldError
import net.liftweb.http.{SHtml, SessionVar, RequestVar}
import net.liftweb.common._

import scala.xml.NodeSeq
import scala.util.matching.Regex

import org.plummtw.astgrail.enum._
import org.plummtw.astgrail.data._
import org.plummtw.astgrail.util.PlummUtil
import org.plummtw.astgrail.heavy.GameProcessor

object CurrentUserEntry   extends SessionVar[Box[UserEntry]](Empty)
object CurrentUserEntry_E extends SessionVar[UserEntry](GlobalUserEntry.NoUserEntry)
object CurrentUserEntry_R extends SessionVar[UserEntry](GlobalUserEntry.NoUserEntry)

//object UserEntrys_E        extends SessionVar[List[UserEntry]](List())
//object UserEntrys_ER       extends SessionVar[List[UserEntry]](List())
//object UserEntrys_R        extends SessionVar[List[UserEntry]](List())
//object UserEntrys_RR       extends SessionVar[List[UserEntry]](List())


class UserEntry extends LongKeyedMapper[UserEntry] with CreatedUpdated with IdPK {
  def getSingleton = UserEntry // what's the "meta" object
  //def primaryKeyField = id

  // the primary key
  //object id            extends MappedLongIndex(this)
  object room_id       extends MappedLongForeignKey(this, Room)
  object team_id       extends MappedLongForeignKey(this, UserEntryTeam)
  object user_id       extends MappedLongForeignKey(this, User)
  
  object user_icon_id  extends MappedLongForeignKey(this, UserIcon) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: Long): List[FieldError] = 
      if (in == 0)  List(FieldError(this, <b>尚未選擇圖像</b>)) 
      else Nil
  }
  
  object user_no       extends MappedInt(this)
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
  
  // trip
  object trip         extends MappedString(this, 20) {
    override def validations = validPriority _ :: super.validations

    def validPriority(in: String): List[FieldError] =
      List(if (in.length() > 20)  List(FieldError(this, <b>ｔｒｉｐ過長＞２０</b>))
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
      SHtml.radio(sex_map.keys.toList, Full(UserEntry.this.sex.toString), UserEntry.this.sex(_))

    def generateHtml = sex_radios.flatMap(PlummUtil.htmlize(_, sex_map))

    def validPriority(in: String): List[FieldError] =
      if (!sex_map.contains(in)) List(FieldError(this, <b>性別錯誤</b>))
      else Nil
  }
  
  object role    extends MappedString(this, 2)
  def get_role =  RoleEnum.get_role(role.is)
  
  def get_role_field(cssclass : String) = get_role.cfield(cssclass)
  
  //2011-10-22 zephyr
  def get_role_rule_link = get_role.rule_link

  
  object tapped        extends MappedBoolean(this) {
    override def defaultValue = false
  }
  
  object yellow_index  extends MappedInt(this) {
    override def defaultValue = 0
  }
  def add_yellow_index(in : Int) = {
    val yellow_index_max = get_role.role_yellow_index_max
    if (yellow_index.is < yellow_index_max)
      yellow_index(math.min(yellow_index.is + in, yellow_index_max))
    this
  }
  def lower_yellow_index(in: Int) = {
    yellow_index(yellow_index.is - in)
    this
  }
  
  object blue_index    extends MappedInt(this) {
    override def defaultValue = 0
  }
  def add_blue_index(in : Int) = {
    val blue_index_max = get_role.role_blue_index_max
    if (blue_index.is < blue_index_max)
      blue_index(math.min(blue_index.is + in, blue_index_max))
    this
  }
  def lower_blue_index(in: Int) = {
    blue_index(blue_index.is - in)
    this
  }
  
  object hand_max      extends MappedInt(this) {
    override def defaultValue = 6
  }
  def add_hand_max(in : Int) = {
    hand_max(hand_max.is + in)
    this
  }
  def lower_hand_max(in: Int) = {
    hand_max(hand_max.is - in)
    this
  }
  
  object fixed_hand_max extends MappedInt(this) {
    override def defaultValue = 0
  }
  def get_hand_max = {
    if (fixed_hand_max.is == 0) {
      val result = hand_max.is
      if (get_role == RoleButterfly)
        math.max(3, hand_max - yellow_index.is)
      else
        result
    } else
      fixed_hand_max.is
  }
  
  object heals       extends MappedInt(this) {
    override def defaultValue = 0
  }
  def add_heals(in : Int) = {
    val role1 = get_role
    val heals_max =
      if (role1 == RoleSaintLance) 3
      else if (role1 == RoleNecromancer) 5
      else if (role1 == RoleBishop) 6
      else if (role1 == RoleRedKnight) 4
      else 2
    if (heals.is < heals_max)
      heals(math.min(heals.is + in, heals_max))
    this
  }
  
  def add_heals(in : Int, max : Int) = {
    if (heals.is < max)
      heals(math.min(heals.is + in, max))
    this
  }
  def lower_heals(in: Int) = {
    heals(heals.is - in)
    this
  }
  
  def energy_max =
    if (get_role == RoleSage) 4
    else 3
  
  object gems          extends MappedInt(this) {
    override def defaultValue = 0
  }
  def add_gems(in : Int) = {
    if (gems.is + crystals.is < energy_max)
      gems(math.min(gems.is + in, energy_max - crystals.is)).save
    this
  }
  
  def lower_gems(in : Int) = {
    gems(gems.is - in)
    this
  }

  object crystals       extends MappedInt(this) {
    override def defaultValue = 0
  }
  def add_crystals(in : Int) = {
    if (gems.is + crystals.is < energy_max)
      crystals(math.min(crystals.is + in, energy_max - gems.is)).save
    this
  }
  
  def lower_crystals(in : Int) = {
    var remains = in
    val lower_num1 = math.min(crystals.is, remains)
    crystals(crystals.is - lower_num1)
    remains = remains - lower_num1
    gems(gems.is - remains)
    this
  }
  
  
  object revoked      extends MappedBoolean(this) {
    override def defaultValue = false
  }
  
  object last_words    extends MappedString(this, 600) {
    override def validations = validPriority _ :: super.validations 
 
    def validPriority(in: String): List[FieldError] = 
      if (in.length() > 600)  List(FieldError(this, <b>遺言過長＞６００</b>))
      else Nil
  }
  
  object ip_address0     extends MappedString(this, 20) with LifecycleCallbacks {
    override def beforeCreate = {
      this(ip_address.is)
    }
  }
  object ip_address     extends MappedString(this, 20) {
    override val defaultValue = ""
  }
  object ip_address_md5 extends MappedString(this, 34) with LifecycleCallbacks {
    override def beforeCreate = {
      this(PlummUtil.generateMD5(ip_address.is))
    }
  }

  object reaction       extends MappedBoolean(this)
  object last_talk      extends MappedString(this, 600)
  
  object target_user    extends MappedLongForeignKey(this, UserEntry)
  
  object room_flags     extends MappedString(this, 20)
  object role_flags     extends MappedString(this, 20)
  object user_flags     extends MappedString(this, 80)
  object card_flags     extends MappedString(this, 80)
  object option1        extends MappedString(this, 50)
  object option2        extends MappedString(this, 50)
  
  def get_user_icon : UserIcon = {
    UserIconCache.getOr(user_icon_id.is) { () => 
      UserIcon.find(By(UserIcon.id, user_icon_id.is)) match {
        case Full(x) => x
        case x       => UserIcon.find(By(UserIcon.id, 1)).get 
   }}}
  
  
  def has_room_flag(flag : UserEntryRoomFlagEnum.Value) : Boolean = 
    return (room_flags.is.indexOf(flag.toString) != -1)
  def hasnt_room_flag(flag : UserEntryRoomFlagEnum.Value) : Boolean = 
    !has_room_flag(flag)
  def add_room_flag(flag : UserEntryRoomFlagEnum.Value) : UserEntry = 
    room_flags(room_flags.is + flag.toString)
  def remove_room_flag(flag : UserEntryRoomFlagEnum.Value) : UserEntry = 
    room_flags(room_flags.is.replace(flag.toString, ""))
  
  def has_role_flag(flag : UserEntryRoleFlagEnum.Value) : Boolean = 
    return (role_flags.is.indexOf(flag.toString) != -1)
  def hasnt_role_flag(flag : UserEntryRoleFlagEnum.Value) : Boolean = 
    !has_role_flag(flag)
  def add_role_flag(flag : UserEntryRoleFlagEnum.Value) : UserEntry = 
    role_flags(role_flags.is + flag.toString)
  def remove_role_flag(flag : UserEntryRoleFlagEnum.Value) : UserEntry = 
    role_flags(role_flags.is.replace(flag.toString, ""))
  
  def has_user_flag(flag : UserEntryFlagEnum.Value) : Boolean = 
    return (user_flags.is.indexOf(flag.toString) != -1)
  def hasnt_user_flag(flag : UserEntryFlagEnum.Value) : Boolean = 
    !has_user_flag(flag)
  def add_user_flag(flag : UserEntryFlagEnum.Value) : UserEntry = 
    user_flags(user_flags.is + flag.toString)
  def remove_user_flag(flag : UserEntryFlagEnum.Value) : UserEntry = 
    user_flags(user_flags.is.replace(flag.toString, ""))
}

object UserEntry extends UserEntry with LongKeyedMetaMapper[UserEntry] with Logger {
  override def fieldOrder = List(id, room_id, team_id, user_id, user_icon_id, user_no, uname, handle_name, trip,  password, sex,
                               role, tapped, yellow_index, blue_index, hand_max, fixed_hand_max, heals,
                               gems, crystals, last_words,revoked,
                               ip_address0, ip_address, ip_address_md5,
                               reaction, last_talk, target_user, room_flags, 
                               role_flags, user_flags, card_flags, option1, option2)

  def get (get_id : Long, userentrys : List[UserEntry]) : UserEntry= {
    if (get_id <= 0) {
      //warn("UserEntry.get : get_id <= 0 : " + get_id.toString)
      get_id match {
        case -1 => GlobalUserEntry.AdminUserEntry
        case  _ => GlobalUserEntry.NoUserEntry
      }
    } else
      userentrys.find(_.id.is == get_id) match {
        case Some(x) => x
        case xs      => warn("UserEntry.get : get_id failed : " + get_id.toString)
                        warn("userentrys : " + userentrys.toString)
                        GlobalUserEntry.NoUserEntry
      }
  }
  
  def getByNo (get_no : Int, userentrys : List[UserEntry]) : UserEntry= {
    if (get_no <= 0) {
      //warn("UserEntry.get : get_no <= 0 : " + get_no.toString)
      get_no match {
        case -1 => GlobalUserEntry.AdminUserEntry
        case  _ => GlobalUserEntry.NoUserEntry
      }
    } else
      userentrys.find(_.user_no.is == get_no) match {
        case Some(x) => x
        case xs      => warn("UserEntry.get : get_no failed : " + get_no.toString)
                        warn("userentrys : " + userentrys.toString)
                        GlobalUserEntry.NoUserEntry
      }
  }
  
  
  def rrnc(currentuserentry : UserEntry, userentrys : List[UserEntry]) =
    userentrys.filter(x =>  (!x.revoked.is) && (x != currentuserentry))
  
  def rr(userentrys : List[UserEntry]) =
    userentrys.filter(!_.revoked.is)
  
  def findAllByRoom (room : Room) : List[UserEntry] = {
    if ((room.status.is == RoomStatusEnum.WAITING.toString)) // ||
        // (room.hasnt_flag(RoomFlagEnum.RANDOM_POSITION)))
      UserEntry.findAll(By(UserEntry.room_id, room.id.is))
    else
      UserEntry.findAll(By(UserEntry.room_id, room.id.is),
                        OrderBy(UserEntry.user_no, Ascending))
  }
}

object GlobalUserEntry{
  val NoUserEntry    = UserEntry.create.handle_name("無").user_icon_id(1)
  val AdminUserEntry = UserEntry.create.handle_name("管理員").user_icon_id(1)
  
  val GLOBAL_USERENTRY_LIST = List(NoUserEntry, AdminUserEntry)
}
