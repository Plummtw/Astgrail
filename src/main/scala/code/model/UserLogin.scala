package org.plummtw.astgrail.model

import net.liftweb._
import net.liftweb.mapper._
import net.liftweb.util.FieldError

import org.plummtw.astgrail.enum.UserLoginEnum
import org.plummtw.astgrail.util.PlummUtil

class UserLogin extends LongKeyedMapper[UserLogin] with CreatedUpdated with IdPK {
  def getSingleton = UserLogin

  object user_id    extends MappedLongForeignKey(this, User)
  object uname      extends MappedString(this,20) {
    override def validations = validPriority _ :: super.validations

    def validPriority(in: String): List[FieldError] =
      List(if (in.length() <  1)       List(FieldError(this, <b>名稱過短＜１</b>))
             else if (in.length() > 20)  List(FieldError(this, <b>名稱過長＞８０</b>))
             else Nil,
           if (PlummUtil.hasHtmlCode(in)) List(FieldError(this, <b>名稱包含控制碼</b>)) else Nil).flatten
  }

  object login_type extends MappedEnum(this, UserLoginEnum)

  object created_ip  extends MappedString(this,20) {
    override def defaultValue = PlummUtil.getIpAddress()
  }
}

object UserLogin extends UserLogin with LongKeyedMetaMapper[UserLogin] {
  override def fieldOrder = List(id, user_id, uname, login_type,
                                   created_ip)

  def create_record(user_id : Long, uname : String, login_type: UserLoginEnum.Value) {
    val login_record = UserLogin.create.user_id(user_id)
                                     .uname(uname).login_type(login_type).save
  }
}