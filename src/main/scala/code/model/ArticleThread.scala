package org.plummtw.astgrail.model 
 
import net.liftweb._ 
import mapper._ 
import http._ 
import SHtml._ 
import util._
import org.plummtw.astgrail.util.PlummUtil
import org.plummtw.astgrail.util.AdminHelper

class ArticleThread extends LongKeyedMapper[ArticleThread]  with CreatedUpdated with IdPK {
  def getSingleton = ArticleThread // what's the "meta" object
  //def primaryKeyField = id
  
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

  object title       extends MappedString(this, 80) {
    override def validations = validPriority _ :: super.validations

    def validPriority(in: String): List[FieldError] =
      if (in.length() < 1)          List(FieldError(this, <b>標題過短＜１</b>))
        else if (in.length() > 80)  List(FieldError(this, <b>標題過長＞８０</b>))
        else List()
  }
  
  object articles extends MappedInt (this)
  
  object deleted  extends MappedBoolean( this )
}

object ArticleThread extends ArticleThread with LongKeyedMetaMapper[ArticleThread] {
  override def fieldOrder = List(id, handle_name, trip, title, articles, deleted)
}