package org.plummtw.astgrail.model

import scala.xml._
import net.liftweb._
import net.liftweb.mapper._
import http._
import js._
import util._
import common._
import S._
import SHtml._
import Helpers._

//import net.liftweb.http.RequestVar
//import net.liftweb.http.SessionVar

//import scala.util.matching.Regex

import org.plummtw.astgrail.enum._
//import org.plummtw.astgrail.util.PlummUtil

//object RoomPhase_E extends SessionVar[RoomPhase](null)
//object RoomPhase_R extends SessionVar[RoomPhase](null)

class RoomPhase extends LongKeyedMapper[RoomPhase] with CreatedUpdated with IdPK {
  def getSingleton = RoomPhase // what's the "meta" object

  object roomround_id  extends MappedLongForeignKey(this, RoomRound)
  
  object phase_no       extends MappedInt(this)
  object phase_type     extends MappedString(this, 5)
  object phase_subtype  extends MappedString(this, 5)
  object actioner_id      extends MappedLongForeignKey(this, UserEntry) 
  object actionee_id      extends MappedLongForeignKey(this, UserEntry) 
  object actionee2_id      extends MappedLongForeignKey(this, UserEntry)
  
  object card_enum       extends MappedString(this, 2)
  object action_card      extends MappedLongForeignKey(this, CardPool)
  object action_cards     extends MappedString(this, 200)
  object action_flags     extends MappedString(this, 200)
  object action_flags2    extends MappedString(this, 200)
  
  object stack            extends MappedBoolean(this) {
    override def defaultValue = true
  }
  object stage            extends MappedInt(this)
  object power            extends MappedInt(this)
  
  object deadline          extends MappedDateTime(this)
  //object additional        extends MappedInt(this)
  //object additional_flag   extends MappedString(this, 1)
  //object attack_no         extends MappedInt(this)
  
  object last_phase_type     extends MappedString(this, 5)
  object last_phase_subtype  extends MappedString(this, 5) 
  object phase_flags         extends MappedString(this, 20)
  //object phase_flags3   extends MappedString(this, 20)
}

object RoomPhase extends RoomPhase with LongKeyedMetaMapper[RoomPhase] with Logger {
  override def fieldOrder = List(id, roomround_id, phase_no, phase_type, phase_subtype,
                                 actioner_id, actionee_id, actionee2_id, card_enum, action_card, action_cards,
                                 action_flags, action_flags2, stack, stage, power, deadline,
                                 last_phase_type, last_phase_subtype, phase_flags)

  def findAllByRoomRoundId(roomround_id : Long) = {
    RoomPhase.findAll(By(RoomPhase.roomround_id, roomround_id),
                      OrderBy(RoomPhase.phase_no, Ascending))
  }
  
  def createFrom(last_phase : RoomPhase) = 
    RoomPhase.create.roomround_id(last_phase.roomround_id).power(last_phase.power.is)
             .phase_flags(last_phase.phase_flags.is)
             .last_phase_type(last_phase.last_phase_type.is).last_phase_subtype(last_phase.last_phase_subtype.is)

  def get_phase(room : Room, roomphases : List[RoomPhase]) = {
    if (room.processing)
      roomphases.find(!_.stack.is).get
    else  
      roomphases.filter(_.stack.is)(room.stack_index)
  }
  

  def copy_phase(to_roomphase : RoomPhase, roomphase : RoomPhase) {
    // 未 copy id roomround_id phase_no stack
    to_roomphase.phase_type(roomphase.phase_type.is).phase_subtype(roomphase.phase_subtype.is)
                .actioner_id(roomphase.actioner_id.is).actionee_id(roomphase.actionee_id.is)
                .actionee2_id(roomphase.actionee2_id.is)
                .card_enum(roomphase.card_enum.is).action_card(roomphase.action_card.is)
                .action_cards(roomphase.action_cards.is)
                .action_flags(roomphase.action_flags.is).action_flags2(roomphase.action_flags2.is)
                .stage(roomphase.stage.is).power(roomphase.power.is).deadline(roomphase.deadline.is)
                .last_phase_type(roomphase.last_phase_type.is).last_phase_subtype(roomphase.last_phase_subtype.is)
                .phase_flags(roomphase.phase_flags.is)
  }
  
  /*
  def push_before(gameo : GameObject, roomphase : RoomPhase) = {
    println("push_before received")
    // stack index = -1 時空的， roomphases_stack 長度應為 0
    // stack index = 0 有 1 個， roomphases_stack 長度應為 1
    val room = gameo.room
    val roomphases_stack = gameo.roomphases.filter(_.stack.is)
    if (room.stack_index.is == roomphases_stack.length - 1) {
      val new_phase_no =
        if (roomphases_stack.length != 0)
          roomphases_stack.last.phase_no.is + 1
        else
          0
        
      roomphase.stack(true).phase_no(new_phase_no).save
      room.stack_index(room.stack_index.is + 1).save
      
      gameo.roomphases = (gameo.roomphases ::: List(roomphase)).sortBy(_.phase_no.is)
    } else {
      val old_roomphase = roomphases_stack(room.stack_index + 1)
      copy_phase(old_roomphase, roomphase)
      old_roomphase.save
      room.stack_index(room.stack_index.is + 1).save
      
      gameo.roomphases = gameo.roomphases.sortBy(_.phase_no.is)
    }
  }
  
  def push_after(gameo : GameObject, roomphase : RoomPhase) = {
    println("push_after received")
    // stack index = -1 時空的， roomphases_stack 長度應為 0
    // stack index = 0 有 1 個， roomphases_stack 長度應為 1
    
    val room = gameo.room
    val roomphases_stack = gameo.roomphases.filter(_.stack.is)
    if (room.stack_index.is == roomphases_stack.length - 1) {
      val old_phase_no =
        if (roomphases_stack.length != 0) {
          val result = roomphases_stack.last.phase_no.is
          roomphases_stack.last.phase_no(result + 1).save
          result
        } else
          0
      
      roomphase.stack(true).phase_no(old_phase_no).save
      room.stack_index(room.stack_index.is + 1).save
      
      gameo.roomphases = (gameo.roomphases ::: List(roomphase)).sortBy(_.phase_no.is)
    } else {
      val old_roomphase = roomphases_stack(room.stack_index + 1)
      copy_phase(old_roomphase, roomphase)
      
      if (room.stack_index.is >= 0) {
        val old_roomphase2 = roomphases_stack(room.stack_index.is)
        val old_roomphase2_phaseno = old_roomphase2.phase_no.is
        old_roomphase2.phase_no(old_roomphase.phase_no.is).save
        
        old_roomphase.phase_no(old_roomphase2_phaseno)
      } else
        old_roomphase.phase_no(0)
      
      old_roomphase.save
      room.stack_index(room.stack_index.is + 1).save
      
      gameo.roomphases = (gameo.roomphases).sortBy(_.phase_no.is)
    }
  }
  */
  
  def push(gameo : GameObject, roomphase : RoomPhase, is_room_save : Boolean = false) = {
    /*
    if (gameo.is_delayed_action)
      push_before(gameo, roomphase)
    else
      push_after(gameo, roomphase)
    */
    val room = gameo.room

    info("Push Received : " + roomphase.toString)
    info("room stack index (before): " + room.stack_index.is)
    // stack index = -1 時空的， roomphases_stack 長度應為 0
    // stack index = 0 有 1 個， roomphases_stack 長度應為 1
    val roomphases_stack = gameo.roomphases.filter(_.stack.is)
    if (roomphases_stack.filter(_.phase_no.is == 0).length > 1) {
      warn("RoomPhases Phase_NO = 0 More Than 1")
      warn("RoomPhases : " + roomphases_stack.toString)
    }
    
    
    if (room.stack_index.is == roomphases_stack.length - 1) {
      val new_phase_no = roomphases_stack.length
        
      roomphase.stack(true).phase_no(new_phase_no).save
      room.stack_index(room.stack_index.is + 1)
      if (is_room_save)
        room.save
      
      gameo.roomphases = (gameo.roomphases ::: List(roomphase)).sortBy(_.phase_no.is)
    } else {
      val old_roomphase = roomphases_stack(room.stack_index + 1)
      copy_phase(old_roomphase, roomphase)
      roomphase.roomround_id(0) // 為安全起見把 roomround_id 設成 0
      old_roomphase.save
      room.stack_index(room.stack_index.is + 1)
      if (is_room_save)
        room.save
      
      gameo.roomphases = gameo.roomphases.sortBy(_.phase_no.is)
    }
    
    if (room.stack_index.is != 0) {
      // 交換之前的 DISCARD_REACTION
      val roomphases_stack = gameo.roomphases.filter(_.stack.is)
      val roomphase_current = roomphases_stack(room.stack_index.is)
      val roomphase_previous = roomphases_stack(room.stack_index.is - 1)
      if ((roomphase_previous.phase_type.is == RoomPhaseEnum.DISCARD_REACTION.toString) && 
          (roomphase_current.phase_type.is != RoomPhaseEnum.DISCARD_REACTION.toString)) {
        val temp_no = roomphase_previous.phase_no.is
        roomphase_previous.phase_no(roomphase_current.phase_no.is).save
        roomphase_current.phase_no(temp_no).save
        gameo.roomphases = gameo.roomphases.sortBy(_.phase_no.is)
      }
    }
    
    info("roomphases (after): " + gameo.roomphases.toString)
    info("room stack index (after): " + room.stack_index.is)
    info("room processing: " + room.processing.is)
  }
  
  def peek(room : Room, roomphases : List[RoomPhase]) : RoomPhase = {
    if (room.stack_index.is == -1) return null
    
    val roomphases_stack = roomphases.filter(_.stack.is)
    roomphases_stack(room.stack_index.is)
  }  
  
  def pop(gameo : GameObject, is_room_save : Boolean = false) : RoomPhase = {
    val room = gameo.room

    info("pop received")
    info("room stack index (before): " + room.stack_index.is)
    
    if (room.stack_index.is == -1) return null
    
    val roomphases_stack = gameo.roomphases.filter(_.stack.is)
    if (roomphases_stack.filter(_.phase_no.is == 0).length > 1) {
      warn("RoomPhases Phase_NO = 0 More Than 1")
      warn("RoomPhases : " + roomphases_stack.toString)
    }
    
    
    val result = roomphases_stack(room.stack_index.is)
    room.stack_index(room.stack_index.is - 1)
    if (is_room_save)
      room.save
    
    gameo.roomphases.find(!_.stack.is) match {
      case Some(x) =>
        x.stack(true).phase_no(result.phase_no.is).save
      case xs => ;
    }
    
    gameo.roomphases = gameo.roomphases.sortBy(_.phase_no.is)
    
    result.phase_no(-1).stack(false).save
    RoomPhaseLog.create_log(gameo.roomround, result)

    info("PoP result : " + result.toString)
    info("roomphases (after): " + gameo.roomphases.toString)
    info("room stack index (after): " + room.stack_index.is)
    info("room processing: " + room.processing.is)
    result
  }
  
  def current(roomphases : List[RoomPhase]) : RoomPhase = {
    roomphases.find(!_.stack.is).get
  }
}
