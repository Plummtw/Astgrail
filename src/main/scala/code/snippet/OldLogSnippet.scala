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

import scala.xml.NodeSeq

import collection.mutable.{LinkedList, HashMap, SynchronizedMap}

import org.plummtw.astgrail.enum._
import org.plummtw.astgrail.model._
import org.plummtw.astgrail.util._
import org.plummtw.astgrail.actor._
//import org.plummtw.astgrail.heavy.CardHelper

class OldLogSnippet {
  
  def list  = {
    val page_no : Int = 
      try { S.param("page_no").getOrElse("0").toInt }
      catch { case e: Exception => 0 }
      
    val filter =
      S.param("filter").getOrElse("")

    def victorys(room : Room) = 
      RoomVictoryEnum.victory_name(room.victory.is) + " " + 
      (if (room.victory_all.is != "")
        room.victory_all.is.split(",").map(RoleEnum.get_role(_).role_name).mkString(",")
       else "")
      
    val room_list = 
      if (filter == "noabandon")
        Room.findAll(By(Room.status, RoomStatusEnum.ENDED.toString),
                     NotBy(Room.victory, RoomVictoryEnum.ABANDONED.toString),
                     OrderBy(Room.id,Descending),
                     StartAt(page_no * 20), MaxRows(20))
      else
        Room.findAll(By(Room.status, RoomStatusEnum.ENDED.toString),
                     OrderBy(Room.id,Descending),
                     StartAt(page_no * 20), MaxRows(20))
   
    val filter_str = 
      if (filter != "")
        "&filter=" + filter
      else ""
    
    val last_page = if (page_no == 0) <span></span>
                    else <a href={"oldlog_list.html?page_no=" + (page_no-1).toString + filter_str}>上一頁</a>
    val next_page = if (room_list.length != 20) <span></span>
                    else <a href={"oldlog_list.html?page_no=" + (page_no+1).toString + filter_str}>下一頁</a>

    val room_table = <table>
      <tr>
        <th class="column">村No</th>
        <th class="column">村名</th>
        <th class="column">村莊說明</th>
        <th class="column">人數</th>
        <th class="column">勝</th>
        <th class="column">選項</th>
      </tr>
      { for (room <- room_list) yield 
      <tr> 
        <td align="right" valign="middle" class="row">{room.id.is.toString}</td> 
        <td align="right" valign="middle" class="row">
         <a href={"oldlog_view.html?room_no="+room.id.is.toString}>{room.room_name.is} 村</a>
        </td> 
        <td align="right" valign="middle" class="row"><small>～ {room.room_comment.is.toString} ～</small></td> 
        <td align="center" valign="middle" class="row">[ {room.max_user.is.toString}人用 ]</td> 
        <td align="center" valign="middle" class="row">[{victorys(room)}]</td>
        <td valign="middle" class="row"><small>{room.option_text}</small></td>
      </tr>
      }
    </table> 
    
    "#last_page"  #>  last_page &
    "#next_page"  #>  next_page &
    "#room_table" #>  room_table
  }
  
  def view = {
    val room_no = 
      try { S.param("room_no").getOrElse("0").toLong }
      catch { case e:Exception => 0}
    var room_box = Room.find(By(Room.id, room_no)) 
    if (room_box.isEmpty) {
      for (room <- CurrentRoom.get) {
        // Reload Room
        room_box = Room.find(By(Room.id, room.id.is))
      }
    }
    
    if (room_box.isEmpty) {
      S.error(<b>找不到村莊</b>)
      S.redirectTo("main.html")
    }
    
    val room = room_box.get
    //Room_R.set(room)
    val room_id = room.id.is
    
    if (room.status.is != RoomStatusEnum.ENDED.toString()) {
      S.error(<b>遊戲尚未結束</b>)
      S.redirectTo("main.html")
    }
    
    val roomround_box  = RoomRound.find(By(RoomRound.room_id, room_id), OrderBy(RoomRound.round_no, Descending))
    if (roomround_box.isEmpty) {
      S.error(<b>找不到遊戲回合</b>)
      S.redirectTo("main.html")
    }
    
    val roomround = roomround_box.get
    println("RoomRound ID : " + roomround.id.is)
    //RoomRound_R.set(roomround)
    
    val roomphase_box  = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is), OrderBy(RoomPhase.phase_no, Descending))
    if (roomphase_box.isEmpty) {
      S.error(<b>找不到遊戲階段</b>)
      S.redirectTo("main.html")
    }
    
    val roomphases = RoomPhase.findAllByRoomRoundId(roomround.id.is)
    val roomphase  = RoomPhase.get_phase(room, roomphases)
    //RoomPhase_R.set(roomphase)
    
    val userentrys    = UserEntry.findAllByRoom(room)
    //UserEntrys_R.set(userentrys)
    val userentrys_rr = UserEntry.rr(userentrys)
    //UserEntrys_RR.set(userentrys_in)
    val userentryteams = UserEntryTeam.findAll(By(UserEntryTeam.room_id, room.id.is))

    
    val card_list = CardPool.findAllByRoomId(room.id.is)
    
    val gameo = new GameObject(room, roomround, roomphases, userentrys, 
                               userentryteams, card_list)
    //GameO_E.set(gameo)
    
    val time_table = 
      if (roomphase.phase_type.is == RoomPhaseEnum.GAMEHALL.toString)
        <span>遊戲大廳</span>
      else if (roomphase.phase_type.is == RoomPhaseEnum.ENDED.toString) {
        val victorys = room.victory_all.is.split(",").map(RoleEnum.get_role(_).role_name).mkString(",")
        <span>遊戲結束，勝利者：{RoomVictoryEnum.victory_name(room.victory.is)} {victorys}</span>
      } else
        <span>第{roomround.round_no.is}日　等待 {UserEntry.get(roomphase.actioner_id.is, userentrys).handle_name.is} {RoomPhaseEnum.get_cname(roomphase, userentrys)}</span>
    
    //val user_table = UserEntryHelper.user_table(room, roomphase, GlobalUserEntry.NoUserEntry, userentrys_in, true)
    //val location_table = LocationHelper.location_table(room, userentrys_in)
    //val talk_table = MessageHelper.messages_all(room.id.is, userentrys, true)
    //val card_table = CardHelper.card_table(room, CardPool.findAll(By(CardPool.room_id, room.id.is), OrderBy(CardPool.card_no, Ascending)))
    
    val user_table = UserEntryHelper.user_table(gameo, GlobalUserEntry.NoUserEntry, true)
    //val location_table = LocationHelper.location_table(room, userentrys)
    val team_table = UserEntryTeamHelper.team_table(room, userentryteams)
    val talk_table = MessageHelper.messages_all(room.id.is, userentrys, true)
    val card_table = CardHelper.card_table(room, null, card_list)
    
    //<meta http-equiv="refresh" content={auto_reload_str} />
    "#room_no"        #> room_no &
    "#room_name"      #> room.room_name.is &
    "#room_comment"   #> room.room_comment.is &
    "name=room_no [value]" #> room_no &
    "#time-table *"      #> time_table &
    "#user-table *"      #> user_table &
    "#card-table *"      #> card_table &
    "#team-table  *"     #> team_table &
    "#talk-table *"      #> talk_table
  }

}
