package org.plummtw.astgrail.model 
 
import net.liftweb._ 
import mapper._ 
import http._ 
import SHtml._ 
import util._
import org.plummtw.astgrail.util.PlummUtil
import org.plummtw.astgrail.util.AdminHelper

class Article extends LongKeyedMapper[Article]  with CreatedUpdated with IdPK {
  def getSingleton = Article // what's the "meta" object
  
  object thread_id       extends MappedLongForeignKey(this, ArticleThread)
  
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
  
  def trip_link =
    if (trip.is != "")
      Seq(<span>◆</span>, <a target="_blank" class="decor_none" href={AdminHelper.trip_link + trip.is}>{trip.is}</a>)
    else
      Seq()
  
  object password      extends MappedString(this, 20) {
    override def validations = validPriority _ :: super.validations

    def validPriority(in: String): List[FieldError] =
      List(if (in.length() < 6)          List(FieldError(this, <b>密碼過短＜６</b>))
             else if (in.length() > 20)  List(FieldError(this, <b>密碼過長＞２０</b>))
             else Nil,
           if (PlummUtil.hasHtmlCode(in)) List(FieldError(this, <b>密碼包含控制碼</b>)) else Nil).flatten
  }
  
  
  
  object content     extends MappedString(this, 3000) {
    override def validations = validPriority _ :: super.validations

    def validPriority(in: String): List[FieldError] =
      if (in.length() < 1)             List(FieldError(this, <b>內容過短＜１</b>))
           else if (in.length() > 3000)  List(FieldError(this, <b>內容過長＞３０００</b>))
           else List()
  }
  
  object deleted  extends MappedBoolean( this )
  
  object edited_times extends MappedInt(this)
  
  object ip_address   extends MappedString(this, 20)
}

object Article extends Article with LongKeyedMetaMapper[Article] {
  override def fieldOrder = List(id, thread_id, uname, handle_name, trip, password, 
                              content, deleted, edited_times, ip_address)
}