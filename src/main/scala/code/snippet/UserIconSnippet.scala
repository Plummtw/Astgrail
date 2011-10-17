package org.plummtw.astgrail.snippet

import _root_.net.liftweb._
import net.liftweb.mapper._
import view.MapperPaginatorSnippet
import http._
import SHtml._
import js._
import JsCmds._
import common._
import util._
import Helpers._

import scala.xml._
import scala.xml.transform._ 

import scala.util.matching.Regex

import org.plummtw.astgrail.enum._
import org.plummtw.astgrail.model._
import org.plummtw.astgrail.util._

import java.io.{File, ByteArrayInputStream, FileOutputStream}
import javax.imageio.ImageIO
object UserIconUploadLock

class UserIconSnippet extends DispatchSnippet { 
  val filename_regex = new Regex("^[\\w]+\\.[A-Za-z]{3}$")
  
  override def dispatch = { 
    case "all" => all 
    //case "top" => top 
    case "paginate"  => paginator.paginate _ 
    //case "paginate2" => paginator.paginate2 _ 
    case "addEntry" => addEntry _
  } 
  
  private val paginator = new MapperPaginatorSnippet(UserIcon) { 
    override def itemsPerPage = 25
    
    override def currentXml: NodeSeq = Text("顯示第 "+(first+1)+" 至 "+(first+itemsPerPage min count)+" 筆 (共： "+count+" 筆)")
    
    def paginate2 = {
      "#first" #> pageXml(0, firstXml) &
      "#prev" #> pageXml(first-itemsPerPage max 0, prevXml) &
      "#allpages" #> {(n:NodeSeq) => this.pagesXml(0 until numPages,n)} &
      "#zoomedpages" #> {(ns: NodeSeq) => this.pagesXml(zoomedPages,ns)} &
      "#next" #> pageXml(first+itemsPerPage min itemsPerPage*(numPages-1) max 0, nextXml) &
      "#last" #> pageXml(itemsPerPage*(numPages-1), lastXml) &
      "#records" #> currentXml &
      "#recordsFrom" #> recordsFrom &
      "#recordsTo" #> recordsTo &
      "#recordsCount" #> count.toString
    }
  } 
   
  def all = "*" #> many(paginator.page) 
  //def top = {
  //  val usericons = UserIcon.findAll
  //  "#icon-table" #> many(usericons)
  //}
  
  private def many(usericons: List[UserIcon]) : Node = 
    <table> {usericons.grouped(5).toList.map { usericon_row => 
      <tr>{ usericon_row.map { usericon => single(usericon) } }</tr>
    } } </table>
  
  private def single(icon: org.plummtw.astgrail.model.UserIcon) : Node = 
    <td class="icon_details"><label for={"icon_" + icon.id.is}><img alt={icon.icon_name.is} src={icon.icon_filename.is} width={icon.icon_width.is.toString} height={icon.icon_height.is.toString} style={"border:3px solid " + icon.color.is}/><br clear="all"/>
      No. {icon.id.is} <br/>
      <font color={"icon_" + icon.color.is}>◆</font>{icon.icon_name.is}</label></td>

  def addEntry(xhtml: NodeSeq): NodeSeq = {
    // Add a variable to hold the FileParamHolder on submission
    var fileHolder  : Box[FileParamHolder] = Empty
    var icon_name   : String = ""

    def doTagsAndSubmit () {
      //CurrentJinrouUser.is match {
      //  case Full(user) => ;
      //  case _          => S.error("尚未登入") ; return
      //}

      val receiptOk = fileHolder match {
        // An empty upload gets reported with a null mime type,
        // so we need to handle this special case
        case Full(FileParamHolder(_, null, _, _)) =>
          S.error("上傳格式無法取得"); false
        case Full(FileParamHolder(_, _, filename, _))
          if (filename_regex.findFirstIn(filename).isEmpty) =>
          S.error("上傳檔名不符"); false
        case Full(FileParamHolder(_, mime, filename, file))
          if mime.startsWith("image/") => {
            val image = ImageIO.read(new ByteArrayInputStream(file))
            println("Width : " + image.getWidth())
            println("Height : " + image.getHeight())
            val color0 = S.param("color0").getOrElse("")
            val color  = (if (S.param("color").getOrElse("on") == "on")
                             color0
                          else S.param("color").getOrElse(""))

            println(new File(".").getAbsolutePath())


            if ((image.getWidth() <= 45) && (image.getHeight() <= 45)) {
              UserIconUploadLock synchronized {
                val usericon = UserIcon.create
                val new_filename = if (filename.length <= 4)
                  filename
                else filename.substring(0, filename.length - 4) + ".png"
                
                val newname1  = "src/main/webapp/upload/" + new_filename
                val newname2  = "target/scala_2.8.1/webapp/upload/" + new_filename
                //val newname3  = "I:/shadowhunter/src/main/webapp/upload/" + new_filename
                val newname3  = "D:/upload/" + new_filename
                println("Filename : " + newname1)
                println("Color : " + color)
                try {
                  if (new File(newname1).exists)
                    throw new RuntimeException("檔案已存在")
                  
                  ImageIO.write(image, "png", new FileOutputStream(newname1))
                  try {
                    ImageIO.write(image, "png", new FileOutputStream(newname2))
                  } catch {case e: Exception => ;}
                  try {
                    ImageIO.write(image, "png", new FileOutputStream(newname3))
                  } catch {case e: Exception => ;}
                  //CurrentJinrouUser.is match {
                  //  case Full(user) => usericon.jinrouuser_id(user.id.is)
                  //  case _          => ;
                  //}
                  usericon.icon_name(icon_name).icon_group(2)
                          .icon_filename("upload/" + new_filename)
                          .icon_width(image.getWidth)
                          .icon_height(image.getHeight)
                          .color(color)

                  // 欄位檢核
                  usericon.validate match {
                    case Nil => usericon.save; S.notice("頭像上傳成功"); true
                    case xs  =>  S.error(xs); false
                  }
                } catch {
                  case e => e.printStackTrace()
                             S.error("檔案存檔失敗：" + e.toString )
                  false
                }
              }
            }
            else {
              S.error("上傳長寬不符（長：" + image.getWidth() + "　寬：" + image.getHeight() + "）")
              false
            }
          }
        case Full(_) =>
          S.error("上傳格式不符"); false
        case _ =>
          S.error("無檔案上傳"); false
      }
    }

    bind("icon", xhtml,
      "upload" -> SHtml.fileUpload(x => fileHolder = Full(x), "size"->"60"),
      "name"    -> SHtml.text(icon_name, x => icon_name = x , "size"->"20", "maxlength"->"20"),
      "submit"  -> SHtml.submit("上傳", doTagsAndSubmit))

  }
}
