package org.plummtw.astgrail.snippet

import _root_.net.liftweb._
import net.liftweb.mapper._
import http._
import SHtml._
import js._
import JsCmds._
import common._
import util._
import Helpers._

import scala.xml._
import scala.xml.transform._ 

import org.plummtw.astgrail.enum._
import org.plummtw.astgrail.model._
import org.plummtw.astgrail.util._
import org.plummtw.astgrail.actor._

// 創新使用者用的 Lock，以免人數超過人數上限
object UserEntryLock {}

class UserEntryRegisterSnippet extends StatefulSnippet {
  // 參數
  private var uname      = ""
  private var handle_name  = ""
  private var password    = ""
  private var trip        = ""
  private var sex        = "M"
  private var align       = ""
  private var role        = ""
  private var player_level = "6"
  private var user_icon       = "0"
  private var user_icon_input = ""
  
  private var icon_table_index : Long = 0L
  private var icon_table_max_index : Long = 0L

  def dispatch = {
    case _ => render
  }

  def render = {
    val room_id = 
      try { S.param("room_no").getOrElse("0").toLong }
      catch { case e:Exception => 0}
    val room_box = Room.find(By(Room.id, room_id))
    
    room_box match {
      case Full(room) if room.status.is == RoomStatusEnum.ENDED.toString() => 
        S.error(<b>遊戲已經結束</b>)
        S.redirectTo("main.html")
      case Full(room) =>  CurrentRoom.set(Box !! room)
      case _         =>
        S.error(<b>找不到村莊</b>)
        S.redirectTo("main.html")
    }
    
    val room = room_box.get
    
    def addIdAttribute(elems: NodeSeq, idPrefix: String, index : Long) : NodeSeq= { 
      elems.map { x => 
        x match {
          case elem : Elem => elem % new UnprefixedAttribute("id",  idPrefix + index, Null)
          case _ => x  
        }
      }
    } 

    def process() : Unit = {
      val user_icon_id : Long =
        try { user_icon.toLong } 
        catch { case e: Exception => 1 }
        
      // 欄位檢核
      val trip_value =
        if (trip == "") "" else PlummUtil.generateSHA1_php(trip.trim()).substring(1,9)
      //println("role : " + S.param("role").getOrElse("0"))
      
      val user_icon_id1 = 
        if (user_icon_id == 0)
          try { user_icon_input.toLong } 
          catch { case e: Exception => 1 }
        else
          user_icon_id
      
      val userentry = UserEntry.create.uname(uname.trim()).handle_name(handle_name.replace('　',' ').trim()).sex(sex)
                                .trip(trip_value).password(password)
                                .room_id(room_id).user_icon_id(user_icon_id1).role_flags(role)
                                .role(align).user_flags(player_level).last_words("")
                                .ip_address(S.request.map{x=>PlummUtil.getIpAddress(x)}.openOr(""))
                      
      
      userentry.validate match {
        case Nil => ;
        case xs  => S.error(xs)
                    return S.redirectTo("room_register.html?&room_no=" + room_id)
      }
      
      UserEntryLock.synchronized {
        // 檢查是否 uname 或 handle_name 重複
        val uname_count = UserEntry.count(By(UserEntry.room_id, room_id), By(UserEntry.uname, uname)) +
                          UserEntry.count(By(UserEntry.room_id, room_id), By(UserEntry.handle_name, handle_name))
        
        if (uname_count > 0) {
          S.error(<b>帳號或暱稱重複</b>)
          return S.redirectTo("room_register.html?&room_no=" + room_id)
        }
      
        // 檢查是否超過人數上限
        val user_count = UserEntry.count(By(UserEntry.room_id, room_id),
                                         By(UserEntry.revoked, false))
        
        if (user_count >= room.max_user.is) {
          S.error(<b>村民已滿</b>)
          return S.redirectTo("room_register.html?room_no=" + room_id)
        }

        GameProcessLock.get_lock(room_id).synchronized {
          Room.find(By(Room.id, room_id)) match {
            case Full(room) if (room.status.is != RoomStatusEnum.WAITING.toString) => {
              S.error(<b>遊戲已經開始</b>)
              S.redirectTo("main.html")
            }
            case xs => ;
          }
          
          val roomround_box   = RoomRound.find(By(RoomRound.room_id, room_id), OrderBy(RoomRound.round_no, Descending))
          roomround_box match {
            case Full(roomround) if (roomround.round_no.is == 0) =>
              // 加入使用者
              userentry.password(PlummUtil.generateSHA1(password.trim()).substring(0,20))
              userentry.save
              CurrentRoom.set(Box !! room)
              CurrentUserEntry.set(Box !! userentry)
        
              val talk = Talk.create.roomround_id(roomround.id.is).actioner_id(userentry.id.is)
                         .mtype(MTypeEnum.MESSAGE_COME.toString).message(userentry.handle_name.is)
              RoomActor ! SessionVarSet(room = room, userentrys = UserEntry.findAllByRoom(room))
              RoomActor ! NewMessage(room, talk)
              RoomActor ! RoomForceUpdate(room_id, List(ForceUpdateEnum.ACTION_BAR, ForceUpdateEnum.USER_TABLE))

              // Update room time
              room.save
              S.redirectTo("login.html?room_no=" + room_id)
            case _ => S.error(<b>找不到遊戲大廳</b>);
          }
        }
      }
    }
    
    //val align_map  = Map("S"->"Shadow", "H"->"Hunter", "N"->"Neutral")
    
    //align = if (room.has_flag(RoomFlagEnum.WISH_ALIGN)) "S"
    //      else ""
      
    
    val sex_map  = Map("M"->"Male", "F"->"Female")
    val sex_radios = SHtml.radio(sex_map.keys.toList, Full(sex), x => sex = x)
    
    //val align_radios = SHtml.radio(align_map.keys.toList, Full(align), x => align = x)
    
    val user_icon_radios = SHtml.radio("0" :: UserIcon.findAll.map(_.id.is.toString), Full(user_icon), x => user_icon = x)
    
    icon_table_max_index = ((UserIcon.count() + 24) / 25) - 1
    
    def user_icon_xhtml(index : Long) : scala.xml.Elem = { 
      val icon_list  = UserIcon.findAll(StartAt(index * 25), MaxRows(25))
      val icon_groups = icon_list.grouped(5)
      
      //<td valign="top"><img src={icon.icon_filename.is} width={icon.icon_width.is.toString} height={icon.icon_height.is.toString} border="2" style={"border-color:" + icon.color.is}/></td>
      //<td width="150px">{icon.icon_name.is}<br/><font color={icon.color.is}>◆</font>{user_icon_radios(icon.id.is.toString)}</td>
      //
      <table><caption>請選擇您的頭像。</caption>
        <tr><td colspan="5">{user_icon_radios(0)}編號：{SHtml.text("", x => user_icon_input = x)}</td></tr> {
        for (icon_group <- icon_groups) yield <tr> { 
          for (icon <- icon_group) yield 
            <td class="icon_details"><label for={"icon_" + icon.id.is}><img alt={icon.icon_name.is} src={icon.icon_filename.is} width={icon.icon_width.is.toString} height={icon.icon_height.is.toString} style={"border:3px solid " + icon.color.is}/><br clear="all"/>
             {addIdAttribute(user_icon_radios(icon.id.is.toString), "icon_", icon.id.is)} No. {icon.id.is} <br/>
             <font color={icon.color.is}>◆</font>{icon.icon_name.is}</label></td>
          } </tr> 
        } 
        </table> 
      }
      
    def process_prev : JsCmd = {
      if (icon_table_index > 0) {
        icon_table_index -= 1
      
        SetHtml("icon-table", user_icon_xhtml(icon_table_index)) &
        SetHtml("icon-table-prev", (if (icon_table_index > 0) ajaxButton("[上一頁]", () => process_prev)
          else <span></span>)) &
        SetHtml("icon-table-next", 
         (if (icon_table_max_index > icon_table_index) ajaxButton("[下一頁]", () => process_next)
          else <span></span>) )
      } else Noop
    }
      
    def process_next : JsCmd = {
      if (icon_table_index < icon_table_max_index) {
        icon_table_index += 1
      
        SetHtml("icon-table", user_icon_xhtml(icon_table_index)) &
        SetHtml("icon-table-prev", ajaxButton("[上一頁]", () => process_prev)) &
        SetHtml("icon-table-next", 
         (if (icon_table_max_index > icon_table_index) ajaxButton("[下一頁]", () => process_next)
          else <span></span>) )
      } else Noop
    }
    
    "#room_no"         #> room_id &
    "#room_name"       #> room.room_name.is &
    "#room_comment"    #> room.room_comment.is &
    "name=uname"       #> SHtml.text(uname,       x => uname = x) & 
    "name=handle_name" #> SHtml.text(handle_name, x => handle_name = x) & 
    "name=password"    #> SHtml.password(password,    x => password = x) & 
    "name=trip"        #> SHtml.text(trip,        x => trip = x) & 
    "#male"            #> sex_radios(0) & 
    "#female"          #> sex_radios(1) &
    "#wish-align"      #> (if (room.has_flag(RoomFlagEnum.WISH_ALIGN))
                            SHtml.select(Seq(("","不指定"),("R","紅隊"),("B","藍隊")),
                              Full(align),  x => align = x) 
                           else
                             SHtml.select(Seq(("","不指定")),
                              Full(align),  x => align = x)) &
    "#wish-role"       #> (if (room.has_flag(RoomFlagEnum.WISH_ROLE))
                             SHtml.select(RoleEnum.WISH_ROLE_LIST.map(x=>(x.toString, RoleEnum.get_role(x.toString).role_name)),
                               Full(role),  x => role = x)
                           else
                             SHtml.select(Seq(("","不指定")),
                               Full(role),  x => role = x))&           
    "#player-level"      #> SHtml.select(Seq(("6","超新手"),("7","新手"),("8","中手"),("9","中高手"),("10","高手")),
                               Full(player_level),  x => player_level = x) &
    //"#shadow"          #> align_radios(0) & 
    //"#hunter"          #> align_radios(1) &
    //"#neutral"         #> align_radios(2) &
    "#icon-table *"      #> user_icon_xhtml(icon_table_index) &
    "#icon-table-prev *" #> <span></span> &
    "#icon-table-next *" #> 
      (if (icon_table_max_index > icon_table_index) ajaxButton("[下一頁]", () => process_next) 
       else <span></span>) &
    "type=submit"      #> SHtml.onSubmitUnit(S.callOnce(process))
  }
}

class UserEntryLoginSnippet {
  
  def render = {
    val room_id = 
      try { S.param("room_no").getOrElse("0").toLong }
      catch { case e:Exception => 0}
    val room_box = Room.find(By(Room.id, room_id))
    
    room_box match {
      case Full(room) if room.status.is == RoomStatusEnum.ENDED.toString() => 
        S.error(<b>遊戲已經結束</b>)
        S.redirectTo("main.html")
      case Full(room) =>
        //CurrentRoom.set(room_box)
        val command = S.param("command").getOrElse("0")
        command match {
          case "logout" => logout(room)
          case "go_out" => go_out(room)
          case  _       => login(room)
        }
      case _          =>
        S.error(<b>找不到村莊</b>)
        S.redirectTo("main.html")
    }
  }
  
  // 登入
  def login(room: Room) = {
    val param_uname    = S.param("uname").getOrElse("").trim()
    val param_password = S.param("password").getOrElse("").trim()
   
    // 輸入 ID 和 PASSWORD 登入
    val userentry_box : Box[UserEntry] =
      if ((param_uname != "") && (param_password != "")) {
        UserEntry.find(By(UserEntry.uname, param_uname), 
                     By(UserEntry.password, PlummUtil.generateSHA1(param_password).substring(0,20)),
                     By(UserEntry.room_id, room.id.is)) match {
          case Full(userentry_loaded) if (!userentry_loaded.revoked) => CurrentRoom.set(Box !! room)
                                                                     //Room_R.set(room)
                                                                     CurrentUserEntry_R.set(userentry_loaded)   
                                                                     CurrentUserEntry.set(Box !! userentry_loaded)
          case _                                                  => Empty
      }
    } else {
      for {
        currentroom      <- CurrentRoom.get;
        currentuserentry <- CurrentUserEntry.get if (currentroom == room);
        userentry_loaded <- UserEntry.find(By(UserEntry.id, currentuserentry.id.is), 
                                         By(UserEntry.room_id, currentroom.id.is))
      } yield userentry_loaded
    }
    
    var (redirect_page, message)  =
      userentry_box match {
        case Full(userentry) =>
          //val dead_mode   = ((!(userentry.live.is)) && (room.status.is != RoomStatusEnum.ENDED.toString))
          //val heaven_mode = ((!(userentry.live.is)) && (room.has_flag(RoomFlagEnum.DEATH_LOOK)))

          // 一登入之後先設初期的 Session 內容        
          //S.setSessionAttribute("dead_mode",   dead_mode.toString)
          //S.setSessionAttribute("heaven_mode", heaven_mode.toString)
          
          //S.setSessionAttribute("auto_reload", "")
          //S.setSessionAttribute("play_sound",  false.toString)
          //S.setSessionAttribute("list_down",   false.toString)

          userentry.ip_address(S.request.map{x=>PlummUtil.getIpAddress(x)}.openOr(""))
          userentry.ip_address_md5(PlummUtil.generateSHA1(userentry.ip_address.is))
          userentry.save
          ("game", "登入完成")
        case _ =>  ("game_view", "觀戰畫面載入中")
      }
    
    val redirect_url = redirect_page + ".html?room_no=" + room.id.is
  
    "#meta_tag"      #> <meta http-equiv="refresh" content={"1;URL=" + redirect_url} /> & 
    "#message_tag"   #> <span>{message}</span> &
    "#a_tag"         #> <a href={redirect_url}>按我繼續</a>
  }
  
  // 登出
  def logout(room: Room) = {
    CurrentRoom.set(Empty)
    CurrentUserEntry.set(Empty)
    
    GameO_E.set(EmptyGameObject)
    GameO_R.set(EmptyGameObject)
    //Room_R.set(null)
    //RoomRound_R.set(null)
    //RoomPhase_R.set(null)
    CurrentUserEntry_E.set(null)
    CurrentUserEntry_R.set(null)
    
    //UserEntrys_R.set(List())
    //UserEntrys_RR.set(List())
    
    if (S.param("force").getOrElse("0") == "1") {
      S.session.open_!.terminateHint 
      S.redirectTo("main.html")
    } else {
      //S.session.open_!.terminateHint 
      S.redirectTo("game_view.html?room_no=" + room.id.is)
    }
  }
  
  // 自刪
  def go_out(room: Room) = {
    UserEntrySnippet.revoke(room, CurrentUserEntry.get.getOrElse(GlobalUserEntry.NoUserEntry))
    logout(room)
  }  
}
                     
object UserEntrySnippet {
  // 踢出
  def revoke(room: Room, userentry : UserEntry) : Unit = {
    if (room.status.is == RoomStatusEnum.WAITING.toString) {
      val userentry_box =
        for {
          //currentroom      <- CurrentRoom.get;
          userentry_loaded <- UserEntry.find(By(UserEntry.id, userentry.id.is), 
                                             By(UserEntry.room_id, room.id.is)) //if (currentroom == room);
          currentroomround <- RoomRound.find(By(RoomRound.room_id, room.id.is), OrderBy(RoomRound.round_no, Descending))
        } yield {
          userentry_loaded.revoked(true).save
          
          val talk = Talk.create.roomround_id(currentroomround.id.is).actioner_id(userentry_loaded.id.is)
               .mtype(MTypeEnum.MESSAGE_LEAVE.toString).message(userentry_loaded.handle_name.is)
          //talk.save()
          RoomActor ! NewMessage(room, talk)
          
          // Update 使用者狀態
          DB.use(DefaultConnectionIdentifier) { conn =>
            // , damaged='0'
            DB.prepareStatement("update UserEntry set room_flags = '' where room_id = ?", conn) { stmt =>
              stmt.setLong(1, room.id.is)
              stmt.executeUpdate()
            }
          }
          
          Action.bulkDelete_!!(By(Action.roomround_id, currentroomround.id.is))
      
          val talk2 = Talk.create.roomround_id(currentroomround.id.is)
                      .mtype(MTypeEnum.MESSAGE_REVOTE0.toString)
          //talk2.save()
          RoomActor ! NewMessage(room, talk2)
          
          RoomActor ! SessionVarSet(room = room, userentrys = UserEntry.findAllByRoom(room))
          RoomActor ! RoomForceUpdate(room.id.is, List(ForceUpdateEnum.ACTION_BAR, ForceUpdateEnum.USER_TABLE))
          
          room.save
        }
      
    }
  }
}
