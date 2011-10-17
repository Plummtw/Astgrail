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

class ArticleThreadSnippet extends DispatchSnippet {
  override def dispatch = { 
    case "all" => all 
    //case "top" => top 
    case "paginate"  => paginator.paginate _ 
    //case "paginate2" => paginator.paginate2 _ 
    //case "addEntry" => addEntry _
  } 
  
  private val paginator = new MapperPaginatorSnippet(ArticleThread) { 
    constantParams = Seq(OrderBy(ArticleThread.updatedAt, Descending))
    
    override def itemsPerPage = 25
    
    override def currentXml: NodeSeq = Text("顯示第 "+(first+1)+" 至 "+(first+itemsPerPage min count)+" 個討論串 (共： "+count+" 個)")
    
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

  private def many(article_threads: List[ArticleThread]) : Node = 
    <table class="table1"> 
      <tr class="table3">
        <td>No.</td>
        <td>標題</td>
        <td>作者</td>
        <td>篇數</td>
        <td>發表時間</td>
        <td>最後時間</td>
      </tr>
      { for (article_thread <- article_threads ) yield 
        single(article_thread) } </table>
  
  private def single(thread_data: org.plummtw.astgrail.model.ArticleThread) : Node = 
    <tr>
      <td>{thread_data.id.is}</td>
      <td><a href={"bbs_view.html?article_no=" + thread_data.id.is}>{thread_data.title.is}</a></td>
      <td>{thread_data.handle_name.is}{thread_data.trip_link}</td>
      <td>{thread_data.articles.is}</td>
      <td>{thread_data.createdAt.is}</td>
      <td>{thread_data.updatedAt.is}</td>
    </tr>
}

class ArticleSnippet {
  def post = {
    // 參數
    var uname       = ""
    var handle_name = ""
    var password    = ""
    var trip        = ""
    var title       = ""
    var content     = ""
    
    def process() = {
      // 欄位檢核
      val trip_value =
        if (trip == "") "" else PlummUtil.generateSHA1_php(trip.trim()).substring(1,9)
      //println("role : " + S.param("role").getOrElse("0"))
      
      val article_thread = ArticleThread.create.handle_name(handle_name.replace('　',' ').trim())
                                         .title(PlummUtil.encodeHtml(title, 80))
                                         .trip(trip_value).articles(1)
                                    
      val article = Article.create.uname(uname.trim()).handle_name(handle_name.replace('　',' ').trim())
                               .trip(trip_value).password(password)
                               .content(PlummUtil.encodeHtml(content, 3000))
                               .ip_address(S.request.map{x=>PlummUtil.getIpAddress(x)}.openOr(""))
                      
      
      article_thread.validate match {
        case Nil => ;
        case xs  => 
           S.error(xs)
          S.redirectTo("bbs_list.html")
      }
      
      article.validate match {
        case Nil => ;
        case xs  => 
          S.error(xs)
          S.redirectTo("bbs_list.html")
      }
      
      article_thread.save
      article.thread_id(article_thread.id.is).save
      
      S.redirectTo("bbs_view.html?article_no=" + article_thread.id.is)
    }
  
    "name=uname"       #> SHtml.text(uname,        x => uname = x) & 
    "name=handle_name" #> SHtml.text(handle_name,  x => handle_name = x) & 
    "name=password"    #> SHtml.password(password, x => password = x) & 
    "name=trip"        #> SHtml.text(trip,         x => trip = x) & 
    "name=title"       #> SHtml.text(title,        x => title = x) & 
    "name=content"     #> SHtml.textarea(content,      x => content = x) & 
    "type=submit"      #> SHtml.onSubmitUnit(S.callOnce(process))
  }
  
  def repost = {
    // 參數
    var uname       = ""
    var handle_name = ""
    var password    = ""
    var trip        = ""
    //var title       = ""
    var content     = ""
    
    val article_thread_id = 
      try { S.param("article_no").getOrElse("0").toLong }
        catch { case e:Exception => 0}
    
    def process() = {
      val article_thread_box = ArticleThread.find(By(ArticleThread.id, article_thread_id))
    
      article_thread_box match {
        case Full(x) => ;
        case xs     => 
          S.error(<b>找不到文章</b>)
          S.redirectTo("bbs_list.html")
      }
    
      val article_thread = article_thread_box.get
            
      // 欄位檢核
      val trip_value =
        if (trip == "") "" else PlummUtil.generateSHA1_php(trip.trim()).substring(1,9)
      //println("role : " + S.param("role").getOrElse("0"))
      
      //val article_thread = ArticleThread.create.handle_name(handle_name.replace('　',' ').trim())
      //                                   .title(PlummUtil.encodeHtml(title, 80))
      //                                   .trip(trip_value).articles(1)
                                    
      val article = Article.create.uname(uname.trim()).handle_name(handle_name.replace('　',' ').trim())
                               .trip(trip_value).password(password)
                               .content(PlummUtil.encodeHtml(content, 3000))
                               .ip_address(S.request.map{x=>PlummUtil.getIpAddress(x)}.openOr(""))
                      
      
      //article_thread.validate match {
      //  case Nil => ;
      //  case xs  => 
      //     S.error(xs)
      //    S.redirectTo("bbs_list.html")
      //}
      
      article.validate match {
        case Nil => ;
        case xs  => 
          S.error(xs)
          S.redirectTo("bbs_list.html")
      }
      
      article_thread.articles(article_thread.articles.is + 1).save
      article.thread_id(article_thread.id.is).save
      
      S.redirectTo("bbs_view.html?article_no=" + article_thread.id.is)
    }
  
    "name=uname"       #> SHtml.text(uname,        x => uname = x) & 
    "name=handle_name" #> SHtml.text(handle_name,  x => handle_name = x) & 
    "name=password"    #> SHtml.password(password, x => password = x) & 
    "name=trip"        #> SHtml.text(trip,         x => trip = x) & 
    //"name=title"       #> SHtml.text(title,        x => title = x) & 
    "name=content"     #> SHtml.textarea(content,      x => content = x) & 
    "type=submit"      #> SHtml.onSubmitUnit(S.callOnce(process))
  }
  
  def view = {
    val article_thread_id = 
      try { S.param("article_no").getOrElse("0").toLong }
      catch { case e:Exception => 0}
    val article_thread_box = ArticleThread.find(By(ArticleThread.id, article_thread_id))
    
    article_thread_box match {
      case Full(x) => ;
      case xs     => 
        S.error(<b>找不到文章</b>)
        S.redirectTo("bbs_list.html")
    }
    
    val article_thread = article_thread_box.get
    val articles = Article.findAll(By(Article.thread_id, article_thread_id))
    
    val article_table =
      <table class="table1">
        <tr class="table3"><td>{Unparsed(article_thread.title.is)}</td></tr>
        {for (article <- articles) yield  
          Seq(<tr class="table3"><td>{article.handle_name.is}{article.trip_link}</td></tr>,
             <tr class="table4"><td>{Unparsed(article.content.is)}</td></tr>,
             <tr class="table2"><td>{article.createdAt.toString}</td></tr>)}
      </table>
    
    "#article-table *" #> article_table
  }
}