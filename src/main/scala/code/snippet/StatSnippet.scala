package org.plummtw.astgrail.snippet

import _root_.net.liftweb._
import net.liftweb.mapper._
import http._
import SHtml._
import js._
import JsCmds._
import js.jquery._
import JqJsCmds._
import common._
import util._
import Helpers._


import scala.xml.NodeSeq

//import collection.mutable.{LinkedList, HashMap, SynchronizedMap}

import org.plummtw.astgrail.enum._
import org.plummtw.astgrail.model._
import org.plummtw.astgrail.util._
import org.plummtw.astgrail.data._
import org.plummtw.astgrail.actor._
import org.plummtw.astgrail.actor._
import org.plummtw.astgrail.card._

import org.plummtw.astgrail.heavy.GameProcessor

class StatSnippet {
  //val stat_counts        = List(10,25,50,100,200,300,400,500)
  def stat = {
  /*
    
    var room_wins_hash = scala.collection.mutable.Map[String, Int]()
    var total_wins     = 0
    
    var role_appears_hash = scala.collection.mutable.Map[String, Int]()
    var role_wins_hash    = scala.collection.mutable.Map[String, Int]()

    def stat_count (rooms : List[Room], userentrys : List[UserEntry], count : Long)  {

      rooms.foreach { room =>
        if (!room.has_flag(RoomFlagEnum.TEST_MODE)) {
          val victory = room.victory.is
          val prev_count =
            if (room_wins_hash.contains(victory)) room_wins_hash.get(victory).get
            else 0
            
          room_wins_hash.put(victory, prev_count + 1)
          
          val room_userentrys = userentrys.filter(x => (x.room_id.is == room.id.is) && (!x.revoked.is))
          
          room_userentrys.foreach { room_userentry =>
            val role = room_userentry.get_real_role
            val role_str = role.role_enum.toString
            
            val role_appear_prev_count =
              if (role_appears_hash.contains(role_str)) role_appears_hash.get(role_str).get
              else 0
            
            role_appears_hash.put(role_str, role_appear_prev_count + 1)
            
            if (GameProcessor.check_user_victory(room_userentry, victory)) {
              val role_win_prev_count =
              if (role_wins_hash.contains(role_str)) role_wins_hash.get(role_str).get
              else 0
            
              role_wins_hash.put(role_str, role_win_prev_count + 1)
            }
          }

          total_wins += 1
          if (total_wins >= count)
            return
        }
      }

      return
    }


    def stat_table (rooms : List[Room], userentrys: List[UserEntry], count : Long) : NodeSeq = {
      stat_count(rooms, userentrys, count)
      if (total_wins == 0)
        NodeSeq.Empty
      else {
        var result : NodeSeq = Seq()
        val room_wins = List(
          RoomVictoryEnum.SHADOW_WIN,
          RoomVictoryEnum.HUNTER_WIN,
          RoomVictoryEnum.DUAL_WIN,
          RoomVictoryEnum.NEUTRAL_WIN,
          RoomVictoryEnum.LOVER_WIN,
          RoomVictoryEnum.DRAW)
        
        result ++= <table>
          <tr><td colspan="3">近 {total_wins} 場資料</td></tr>
          <tr><td>陣營</td><td>勝利次數</td><td>勝率</td></tr>
          { for (room_win <- room_wins) yield {
            val wins = room_wins_hash.get(room_win.toString).getOrElse(0)
            <tr><td>{RoomVictoryEnum.victory_name(room_win)}勝</td>
                <td>{wins}</td><td>{wins*100.0/total_wins}%</td>
            </tr>
          }  
        } </table>
      
        result ++= <table>
          <tr><td colspan="5">各職業出場率及勝率</td></tr>
          <tr><td>職業</td><td>出場次數</td><td>出場率</td><td>勝利次數</td><td>勝率</td></tr>
          { for (room_role <- RoleEnum.ALL_ROLE_LIST) yield {
            val role_appear = role_appears_hash.get(room_role.toString).getOrElse(0)
            val role_win    = role_wins_hash.get(room_role.toString).getOrElse(0)
            val role_win_ratio = 
              if (role_appear == 0 ) 0.0
              else role_win*100.0/role_appear
              <tr><td>{RoleEnum.get_role(room_role).role_name}</td>
                <td>{role_appear}</td><td>{role_appear*100.0/total_wins}%</td>
                <td>{role_win}</td><td>{role_win_ratio}%</td>
            </tr>
          }  
        } </table>
      
        result
      }
    }
    
    var count_room = 
      try { S.param("count").getOrElse("10").toLong }
      catch { case e:Exception => 10}
      
    count_room = math.min(200, math.max(10, count_room))

    val rooms = Room.findAll(NotBy(Room.victory, RoomVictoryEnum.NONE.toString),
                             NotBy(Room.victory, RoomVictoryEnum.ABANDONED.toString),
                             MaxRows(count_room * 2),
                             OrderBy(Room.id, Descending))
    val userentrys = UserEntry.findAll(By_>(UserEntry.room_id, rooms.last.id.is - 1))
    */

    "#stat-table *" #> <span></span> //stat_table(rooms, userentrys, count_room)
  }
}
