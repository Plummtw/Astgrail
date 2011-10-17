package org.plummtw.astgrail.actor

import _root_.net.liftweb._
import net.liftweb.mapper._
import http._
import SHtml._
import js._
import JsCmds._
import common._
import util._
import Helpers._
import actor._

import scala.xml.NodeSeq

import collection.mutable.{LinkedList, HashMap, SynchronizedMap}

import org.plummtw.astgrail.enum._
import org.plummtw.astgrail.model._
import org.plummtw.astgrail.util._

import java.util.concurrent.ScheduledFuture

import org.plummtw.astgrail.heavy.GameProcessor

case class Tick(room_id : Long)
case class RoomTick(room_id : Long)

case class Timeout(room_id : Long, userentry_id : Long)
case class TickPlayer(room_id : Long, userentry_id : Long, count_down : Long)

object ClockActor extends LiftActor with Logger {
  class ClockActorData(var room: Room, var roomphase: RoomPhase,
                       var scheduled : ScheduledFuture[Unit])
  
  private val clock_message_map : HashMap[Long, ClockActorData] = new HashMap()
  
  override def messageHandler = (in:Any) => in match {
    case SessionVarSet(new_room, new_roomround, new_roomphases, 
                       new_userentrys, new_userentryteams, new_card_list) =>
      println("ClockActor SessionVarSetGot")
      val data : ClockActorData = clock_message_map.get(new_room.id.is) match {
        case Some(x) => x
        case x       =>
          val new_data = new ClockActorData(null, null, null)
          clock_message_map.put(new_room.id.is, new_data)
          new_data
      }
      if (new_room != null) 
        data.room = new_room
      if (!new_roomphases.isEmpty) {
        val new_roomphase = RoomPhase.get_phase(new_room, new_roomphases)
        data.roomphase = new_roomphase
        
        // 啟動 Timer
        if (new_roomphase.phase_type.is == RoomPhaseEnum.GAMEHALL.toString) {
          val scheduled : ScheduledFuture[Unit] = Schedule.schedule(this, RoomTick(new_room.id.is), 60000L)
       
          if (data.scheduled != null)
            data.scheduled.cancel(false)
          
          println("ClockActor RoomTick Scheduled")
          data.scheduled = scheduled
        } else if (new_roomphase.phase_type.is != RoomPhaseEnum.ENDED.toString) {
          val scheduled : ScheduledFuture[Unit] = Schedule.schedule(this, Tick(new_room.id.is), 1000L)
       
          if (data.scheduled != null)
            data.scheduled.cancel(false)
          
          println("ClockActor Tick Scheduled")  
          data.scheduled = scheduled
        
        } else {
          if (data.scheduled != null)
            data.scheduled.cancel(false)
          clock_message_map.remove(new_room.id.is)
        }
      }
    case RoomTick(room_id) =>
      println("ClockActor RoomTick")
      val now = new java.util.Date()
      clock_message_map.get(room_id) match {
        case Some(data) => 
          val deadline = data.roomphase.deadline.is
          val count_down = (deadline.getTime - now.getTime) / 1000
          if (count_down < 0 ) {
            //RoomActor ! Timeout(room_id, data.roomphase.player.is)
            /*
            data.room.status(RoomStatusEnum.ENDED.toString).victory(RoomVictoryEnum.ABANDONED.toString)
            data.room.save
            
            RoomActor ! SessionVarSet(room = data.room)
            RoomActor.sendRoomMessage(room_id, RoomForceUpdate(room_id ,List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.ACTION_BAR)))
            */
           GameProcessor.abandon(data.room)
          } else {
            val scheduled : ScheduledFuture[Unit] = 
              Schedule.schedule(this, RoomTick(room_id), 60000L)
            data.scheduled = scheduled
          }  
        case x       => ; 
      }
      
    case Tick(room_id) =>
      println("ClockActor Tick")  
      val now = new java.util.Date()
      clock_message_map.get(room_id) match {
        case Some(data) => 
          if (data.roomphase == null) 
            warn("ClockActor RoomPhase Null") 
          else {
            var deadline = data.roomphase.deadline.is
            if (deadline == null) {
              warn("ClockActor DeadLine null : " + data.roomphase.toString)
              deadline = PlummUtil.dateAddSecond(new java.util.Date(), 60)
              //data.roomphase.deadline(deadline).save
            } // else {
            val count_down = (deadline.getTime - now.getTime) / 1000
            if (count_down < 0 )
              RoomActor ! Timeout(room_id, data.roomphase.actioner_id.is)
            else {
              RoomActor ! TickPlayer(room_id, data.roomphase.actioner_id.is, count_down)
            
              val time_diff = (deadline.getTime - now.getTime)
              val scheduled_time = 
                if (time_diff > 180000)
                  60000L
                else if (time_diff > 60000)
                  20000L
                else if (time_diff > 15000)
                  5000L
                else
                  1000L
              val scheduled : ScheduledFuture[Unit] = 
                Schedule.schedule(this, Tick(room_id), scheduled_time)
              data.scheduled = scheduled
            }
          }  
        case x       => ; 
      }
      
  }
}