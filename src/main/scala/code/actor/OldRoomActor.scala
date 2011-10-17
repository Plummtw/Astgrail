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

//import scala.xml.NodeSeq

import collection.mutable.HashMap

import org.plummtw.astgrail.enum._
import org.plummtw.astgrail.model._
import org.plummtw.astgrail.util._
import org.plummtw.astgrail.comet._
import org.plummtw.astgrail.data._
import org.plummtw.astgrail.heavy._

/*
case class RoomSubscribe(comet : GameComet, room_id : Long, userentry_id : Long)
case class RoomUnsubscribe(comet : GameComet, room_id : Long, userentry_id : Long)
case class RoomDeleteRoom(room_id : Long)

case class NewMessage(room : Room, talk : Talk)
case class SignalAction(action : Action)
case class SignalAbandon(room : Room)
case class SignalDeadline(room : Room, roomround : RoomRound, roomphase : RoomPhase)

case class RoomForceOut(room_id : Long)
case class ForceOut(userentry_id : Long)
case class ForceLogOut(userentry_id : Long)

case class SessionVarSet(room : Room = null, roomround : RoomRound = null, //roomphase : RoomPhase = null, 
                         roomphases : List[RoomPhase] = List(), userentrys : List[UserEntry] = List(),
                         userentryteams : List[UserEntryTeam] = List(), card_list : List[CardPool] = List())
case class RoomForceUpdate(room_id : Long, updates : List[ForceUpdateEnum.Value])
case class UserEntryForceUpdate(userentry_id : Long, updates : List[ForceUpdateEnum.Value])

//case class NewRoomRound(room_id : Long, roomround : RoomRound)

object OldRoomActor extends LiftActor with Logger {
  private val userentry_message_map : HashMap[Long, GameComet] = new HashMap()
  private val reverse_map : HashMap[GameComet, (Long, Long)] = new HashMap()
  
  def sendUserEntryMessage(id : Long, message : Any) = {
    val comet_list = userentry_message_map.get(id) match {
      case Some(some_userentry) => some_userentry ! message
      case _                    => warn("No UserEntry Found")
    }
  }
    
  private val room_message_map : HashMap[Long, List[GameComet]] = new HashMap()
  def sendRoomMessage(id : Long, message : Any) = {
    val comet_list = room_message_map.get(id) match {
      case Some(message_list) => println(message_list) ; message_list.foreach(_ ! message)
      case _                  => warn("No Room Found")
    }
    
    message match {
      case SessionVarSet(room, roomround, roomphases, userentrys,
                         userentryteams, card_list) =>
        ClockActor ! message
      case x => ;
    }
  }
  
  def process_signal_action(action : Action) = {
    info("SignalAction : " + action.mtype.is.toString)
      //val roomround_a = RoomRound.find(By(RoomRound.id, action.roomround_id.is)).get
    val actioner    = UserEntry.find(By(UserEntry.id, action.actioner_id.is)).get
    val room        = Room.find(By(Room.id, actioner.room_id.is)).get
    val roomround   = RoomRound.find(By(RoomRound.room_id, room.id.is),
                                     OrderBy(RoomRound.round_no, Descending)).get
    val roomphases = RoomPhase.findAllByRoomRoundId(roomround.id.is)
    val roomphase  = RoomPhase.get_phase(room, roomphases)
    val userentrys = UserEntry.findAllByRoom(room)
    val userentryteams = UserEntryTeam.findAll(By(UserEntryTeam.room_id, room.id.is))
    val actioner_reload = UserEntry.get(actioner.id.is, userentrys)
    val card_list  = CardPool.findAllByRoomId(room.id.is)
    val gameo = new GameObject(room, roomround, roomphases, //roomphasestack,
                               userentrys, userentryteams, card_list)
      
    var enabled_action_list = ActionHelper.enabled_action_list(gameo, actioner_reload)
    action.save
      
    //if (enabled_action_list.contains(ActionAttack)) {
    //  enabled_action_list = enabled_action_list ::: List(ActionMultiAttack, ActionNoAttack)
    //}
      
    //if (enabled_action_list.contains(ActionCardChoose)) {
    //  enabled_action_list = enabled_action_list ::: List(ActionDrawBlackCard, ActionDrawWhiteCard, ActionDrawGreenCard)
    //}
        
    // 這裡加入檢核      
    if (enabled_action_list.map(_.action_enum.toString).contains(action.mtype.toString)) {
      val talk = Talk.create.roomround_id(action.roomround_id.is).mtype(action.mtype.is)
                            .actioner_id(action.actioner_id.is).actionee_id(action.actionee_id.is)
                            .message_flags(action.action_flags.is).message_flags2(action.action_flags2.is)
      talk.save
      
      sendRoomMessage(roomround.room_id.is, NewMessage(room, talk))
      if ((action.mtype.is == MTypeEnum.ACTION_KICK.toString) ||
          (action.mtype.is == MTypeEnum.ACTION_STARTGAME.toString))
        GameProcessor.process_room(gameo, action)
      else
        GameProcessor.process_main(gameo, action)
    } else {
      warn("Room : " + room)
      warn("RoomRound : " + roomround)
      warn("RoomPhase : " + roomphase)
      warn("Actioner : " + actioner_reload)
      warn("UserEntrys : " + userentrys)
        
      warn("Check Failed :  " )
      warn("Action : " + action.mtype.toString)
      warn("Enabled : " + enabled_action_list.map(_.action_enum.toString).mkString(","))
        
    }
  }
  
  override def messageHandler = (in:Any) => in match {
    case RoomSubscribe(comet, room_id, userentry_id) =>
      info("RoomSubscribe : " + comet.toString + " " + room_id + " " + userentry_id)
      var comet_list = room_message_map.get(room_id) match {
        case Some(message_list) => message_list
        case _                  => List()
      }
      if (userentry_message_map.contains(userentry_id)) {
        val old_comet = userentry_message_map.get(userentry_id).get
        if (old_comet != comet) {
          old_comet ! ForceLogOut(userentry_id)
          comet_list = comet_list filterNot (_ == old_comet)
        }
      }
      if (reverse_map.contains(comet)) {
        val (old_room_id, old_userentry_id) = reverse_map.get(comet).get
        if (old_room_id != room_id) {
          val old_comet_list = room_message_map.get(old_room_id) match {
            case Some(message_list) => message_list
            case _                  => List()
          }
          room_message_map.put(old_room_id, old_comet_list filterNot (_ == comet))
        }
        
        if (old_userentry_id != userentry_id) {
          userentry_message_map.remove(old_userentry_id)
        }
      }
      if (!comet_list.contains(comet))
        room_message_map.put(room_id, comet :: comet_list)
      userentry_message_map.put(userentry_id, comet)
      reverse_map.put(comet, (room_id, userentry_id))
    case RoomUnsubscribe(comet, room_id, userentry_id) =>
      info("RoomUnscribe : " + comet.toString + " " + room_id + " " + userentry_id)
      val comet_list = room_message_map.get(room_id) match {
        case Some(message_list) => message_list
        case _                  => List()
      }
      room_message_map.put(room_id, comet_list filterNot (_ == comet))
      userentry_message_map.remove(userentry_id)
      reverse_map.remove(comet)
    case RoomDeleteRoom(room_id) =>
      info("DeleteRoom : " + room_id)
      room_message_map.remove(room_id)
      
    case NewMessage(room, talk) =>
      info("NewMessage")
      val room_id = room.id.is
      talk.save
      sendRoomMessage(room_id, in)
      //RoomRound.find(By(RoomRound.room_id, room_id), OrderBy(RoomRound.round_no, Descending)) 
      RoomRound.find(By(RoomRound.id, talk.roomround_id.is)) match {
        case Full(roomround) =>
          if (roomround.round_no.is == 0) {
            // 更新廢村時間
            val roomphases = RoomPhase.findAllByRoomRoundId(roomround.id.is)
            // Room.find(By(Room.id, room_id)).get
            val roomphase  = RoomPhase.get_phase(room, roomphases)
            roomphase.deadline(PlummUtil.dateAddMinute(new java.util.Date(), 10)).save
          }
        case xs => ;
      }
      
      //room.talk_time(new java.util.Date()).save
      
    case SignalAction(action) =>
      process_signal_action(action)
    case SessionVarSet(room, roomround, roomphases, userentrys,
                       userentryteams, card_list) =>
      info("SessionVarSet")
      sendRoomMessage(room.id.is, in)
      
    //case NewRoomRound(room_id, roomround) =>
    //  roomround.save
    //  sendRoomMessage(room_id, ForceUpdate(room_id, (List(ForceUpdateEnum.ACTION_BAR, ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.LOCATION_TABLE, ForceUpdateEnum.TALK_TABLE))))
    case RoomForceUpdate(room_id, updates) =>
      info("RoomForceUpdate")
      sendRoomMessage(room_id, in)
    case UserEntryForceUpdate(userentry_id, updates) =>
      info("UserEntryForceUpdate")
      sendUserEntryMessage(userentry_id, in)
      
    case TickPlayer(room_id, userentry_id, count_down) =>
      info("TickPlayer")
      sendUserEntryMessage(userentry_id, in)
    case Timeout(room_id, userentry_id) => 
      info("RoomActor Timeout")
      //println("RoomActor Timeout")  
      //sendUserEntryMessage(userentry_id, in)
      val room = Room.find(By(Room.id, room_id)).get
      val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
                                     OrderBy(RoomRound.round_no, Descending)).get
      val roomphases = RoomPhase.findAllByRoomRoundId(roomround.id.is)
      //val roomphasestack = RoomPhaseStack.findAll(By(RoomPhaseStack.roomround_id, room.id.is))
      val roomphase  = RoomPhase.get_phase(room, roomphases)
      
      val userentrys = UserEntry.findAllByRoom(room)
      val userentryteams = UserEntryTeam.findAll(By(UserEntryTeam.room_id, room.id.is))
      val card_list  = CardPool.findAllByRoomId(room.id.is)
      val gameo = new GameObject(room, roomround, roomphases, //roomphasestack,
                                 userentrys, userentryteams, card_list)
      
      // 這裡加入檢核      
      if (roomphase.actioner_id.is == userentry_id) {
        if ((roomphase.phase_type.is != RoomPhaseEnum.GAMEHALL.toString) &&
            (roomphase.phase_type.is != RoomPhaseEnum.ENDED.toString)) {
          info("RoomActor Process Timeout")
          
          //var is_dead = false
          //var is_abandon = false
          
          val userentry = UserEntry.get(userentry_id, userentrys)
          if (userentry.hasnt_room_flag(UserEntryRoomFlagEnum.AUTOVOTED)) {
            userentry.add_room_flag(UserEntryRoomFlagEnum.AUTOVOTED)
            userentry.save
          } else {
            //userentry.location("").live(false)
            if (userentry.hasnt_room_flag(UserEntryRoomFlagEnum.SKIPPED)) {
              userentry.add_room_flag(UserEntryRoomFlagEnum.SKIPPED)
              userentry.save
            }
            //is_dead = true
          }
          
          val live_userentrys = userentrys.filter(x => (!x.revoked.is) && (x.hasnt_room_flag(UserEntryRoomFlagEnum.AUTOVOTED)))
          if (live_userentrys.length == 0) {
            val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.MESSAGE_TIMEOUT.toString)
                                .actioner_id(userentry_id)
            //if (is_dead) 
            //  talk.mtype(MTypeEnum.MESSAGE_DEATHSUDDEN.toString)
            talk.save
            sendRoomMessage(roomround.room_id.is, NewMessage(room, talk))
            
            GameProcessor.abandon(room)
            /*
            val new_roomround = RoomRound.create.room_id(room.id.is).round_no(roomround.round_no.is + 1)
                                         .last_round(roomround.id.is)
            new_roomround.save
            val talk = Talk.create.roomround_id(new_roomround.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                     .message("遊戲結束 "+ (new java.util.Date).toString)
            talk.save
     
            val new_phase = RoomPhase.create.roomround_id(new_roomround.id.is).phase_no(0)
                                     .phase_type(RoomPhaseEnum.ENDED.toString)
            new_phase.save
             
            room.status(RoomStatusEnum.ENDED.toString).victory(RoomVictoryEnum.ABANDONED.toString)
            room.save
            
            RoomActor ! SessionVarSet(room = room, roomround = new_roomround, roomphase = new_phase)
            //RoomActor.sendRoomMessage(room_id, RoomForceUpdate(room_id ,List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.ACTION_BAR)))
            RoomActor.sendRoomMessage(room_id, RoomForceOut(room_id))
            */
          } else if (!GameProcessor.check_victory(gameo)) {
            val currentuserentry = UserEntry.get(roomphase.actioner_id.is, userentrys)
            val actions = ActionHelper.action_list(gameo, currentuserentry)
            
            if ((roomphase.phase_type.is == RoomPhaseEnum.MAIN.toString) ||
                (roomphase.phase_type.is == RoomPhaseEnum.ACTIVATE.toString) ||
                (roomphase.phase_type.is == RoomPhaseEnum.MAIN_NO_ACTIVATE.toString) ||
                (roomphase.phase_type.is == RoomPhaseEnum.ATTACK.toString) ||
                (roomphase.phase_type.is == RoomPhaseEnum.MAGIC.toString) ||
                (roomphase.phase_type.is == RoomPhaseEnum.ATTACK_OR_MAGIC.toString)) {
              
               val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.MESSAGE_TIMEOUT.toString)
                                     .actioner_id(userentry_id)
               talk.save
               sendRoomMessage(roomround.room_id.is, NewMessage(room, talk))

               gameo.set_next_turn(true)
               GameProcessor.process_main(gameo, null)
               //GameProcessor.next_player(gameo)
            } else if (actions.contains(ActionNoAction)) {
               val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.MESSAGE_AUTO.toString)
                                     .actioner_id(userentry_id)
               talk.save
               sendRoomMessage(roomround.room_id.is, NewMessage(room, talk))
               
               val action = Action.create.roomround_id(gameo.roomround.id.is).actioner_id(currentuserentry.id.is)
                                  .mtype(MTypeEnum.ACTION_NO_ACTION.toString)
               process_signal_action(action)
              
            } else if (actions.contains(ActionEndureAttack)) {
               val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.MESSAGE_AUTO.toString)
                                     .actioner_id(userentry_id)
               talk.save
               sendRoomMessage(roomround.room_id.is, NewMessage(room, talk))
               
               val action = Action.create.roomround_id(gameo.roomround.id.is).actioner_id(currentuserentry.id.is)
                                  .mtype(MTypeEnum.ACTION_ENDUREATTACK.toString)
               process_signal_action(action)
            } else if (actions.contains(ActionEndureMagic)) {
               val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.MESSAGE_AUTO.toString)
                                     .actioner_id(userentry_id)
               talk.save
               sendRoomMessage(roomround.room_id.is, NewMessage(room, talk))
               
               val action = Action.create.roomround_id(gameo.roomround.id.is).actioner_id(currentuserentry.id.is)
                                  .mtype(MTypeEnum.ACTION_ENDUREMAGIC.toString)
               process_signal_action(action)
              
            } else if (roomphase.phase_type.is == RoomPhaseEnum.HEAL_REACTION.toString) {
               val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.MESSAGE_AUTO.toString)
                                     .actioner_id(userentry_id)
               talk.save
               sendRoomMessage(roomround.room_id.is, NewMessage(room, talk))
               
               val action = Action.create.roomround_id(gameo.roomround.id.is).actioner_id(currentuserentry.id.is)
                                  .mtype(MTypeEnum.ACTION_HEAL.toString).action_flags("0")
               process_signal_action(action)
              
            } else if (roomphase.phase_type.is == RoomPhaseEnum.DISCARD_REACTION.toString) {
               val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.MESSAGE_AUTO.toString)
                                     .actioner_id(userentry_id)
               talk.save
               sendRoomMessage(roomround.room_id.is, NewMessage(room, talk))
               
               val cards_in_hand = CardPool.in_hand(currentuserentry, gameo.card_list)
               val discard_number = math.max(0, cards_in_hand.length - currentuserentry.get_hand_max)
               
               val discarded_cards = cards_in_hand.take(discard_number)
               
               val action = Action.create.roomround_id(gameo.roomround.id.is).actioner_id(currentuserentry.id.is)
                                  .mtype(MTypeEnum.ACTION_DISCARD.toString)
                                  .action_flags(discarded_cards.map(_.to_card.card_enum.toString).mkString(","))
                                  .action_cards(discarded_cards.map(_.card_no.toString).mkString(","))  
                                                   
               process_signal_action(action)
              
            } else if (roomphase.phase_type.is == RoomPhaseEnum.BACKDISCARD_REACTION.toString) {
               val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.MESSAGE_AUTO.toString)
                                     .actioner_id(userentry_id)
               talk.save
               sendRoomMessage(roomround.room_id.is, NewMessage(room, talk))
               
               val cards_in_back = CardPool.in_back(currentuserentry, gameo.card_list)
               val discard_number = math.max(0, cards_in_back.length - currentuserentry.get_role.role_back_cards_max)
               
               val discarded_cards = cards_in_back.take(discard_number)
               
               val action = Action.create.roomround_id(gameo.roomround.id.is).actioner_id(currentuserentry.id.is)
                                  .mtype(MTypeEnum.ACTION_BUTTERFLY_BACKDISCARD.toString)
                                  .action_flags(discarded_cards.map(_.to_card.card_enum.toString).mkString(","))
                                  .action_cards(discarded_cards.map(_.card_no.toString).mkString(","))  
                                                   
               process_signal_action(action)
              
            } else if (roomphase.phase_type.is == RoomPhaseEnum.ANGELBLESS_REACTION.toString) {
               val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.MESSAGE_AUTO.toString)
                                     .actioner_id(userentry_id)
               talk.save
               sendRoomMessage(roomround.room_id.is, NewMessage(room, talk))
               
               val cards_in_hand = CardPool.in_hand(currentuserentry, gameo.card_list)
               val give_number = math.min(cards_in_hand.length, gameo.roomphase.power.is)
               
               val give_cards = cards_in_hand.take(give_number)
               
               val action = Action.create.roomround_id(gameo.roomround.id.is).actioner_id(currentuserentry.id.is)
                                  .actionee_id(gameo.roomphase.actionee_id.is)
                                  .mtype(MTypeEnum.ACTION_ANGEL_ANGELBLESS_GIVE.toString)
                                  .action_flags(give_cards.map(_.to_card.card_enum.toString).mkString(","))
                                  .action_cards(give_cards.map(_.card_no.toString).mkString(","))  
                                                   
               process_signal_action(action)
              
            } else if (roomphase.phase_type.is == RoomPhaseEnum.AIRRUNE_REACTION.toString) {
               val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.MESSAGE_AUTO.toString)
                                     .actioner_id(userentry_id)
               talk.save
               sendRoomMessage(roomround.room_id.is, NewMessage(room, talk))
               
               val cards_in_hand = CardPool.in_hand(currentuserentry, gameo.card_list)
               val give_cards    = cards_in_hand(0)
               
               val action = Action.create.roomround_id(gameo.roomround.id.is).actioner_id(currentuserentry.id.is)
                                  .actionee_id(gameo.roomphase.actionee_id.is)
                                  .mtype(MTypeEnum.ACTION_RUNEMAGE_AIRRUNE_DISCARD.toString)
                                  .action_card2(give_cards.id.is).action_flags(give_cards.card.is)
                                  //.action_flags(give_cards.map(_.to_card.card_enum.toString).mkString(","))
                                  //.action_cards(give_cards.map(_.card_no.toString).mkString(","))  
                                                   
               process_signal_action(action)
              
            } else {
               val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.MESSAGE_TIMEOUT.toString)
                                     .actioner_id(userentry_id)
               talk.save
               sendRoomMessage(roomround.room_id.is, NewMessage(room, talk))

               gameo.set_next_turn(true)
               GameProcessor.process_main(gameo, null)
               //GameProcessor.next_player(gameo)
            }
                
          }
        }
      }
  }
}
*/