package org.plummtw.astgrail.util

import scala.xml._
import net.liftweb._
import net.liftweb.mapper._
import http._
import js._
import util._
import S._
import SHtml._
import Helpers._


import org.plummtw.astgrail.model._
import org.plummtw.astgrail.enum._
import org.plummtw.astgrail.actor._
import org.plummtw.astgrail.heavy._
import org.plummtw.astgrail.snippet._
import org.plummtw.astgrail.data._
import org.plummtw.astgrail.card._

object DeadlineHandler {
  def process_deadline(gameo : GameObject) = {
    val room = gameo.room
    val roomround = gameo.roomround
    val roomphase = gameo.roomphase
    val userentry_id = roomphase.actioner_id.is
    val currentuserentry = UserEntry.get(userentry_id, gameo.userentrys)
    val actions = ActionHelper.action_list(gameo, currentuserentry)
    val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.MESSAGE_AUTO.toString)
                         .actioner_id(userentry_id)
    val action = Action.create.roomround_id(gameo.roomround.id.is).actioner_id(currentuserentry.id.is)
     
    if ((roomphase.phase_type.is == RoomPhaseEnum.MAIN.toString) ||
        (roomphase.phase_type.is == RoomPhaseEnum.ACTIVATE.toString) ||
        (roomphase.phase_type.is == RoomPhaseEnum.MAIN_NO_ACTIVATE.toString) ||
        (roomphase.phase_type.is == RoomPhaseEnum.ATTACK.toString) ||
        (roomphase.phase_type.is == RoomPhaseEnum.MAGIC.toString) ||
        (roomphase.phase_type.is == RoomPhaseEnum.ATTACK_OR_MAGIC.toString)) {

       talk.mtype(MTypeEnum.MESSAGE_TIMEOUT.toString)
       talk.save
       talk.send(room)
       gameo.set_next_turn(true)
       GameProcessor.process_main(gameo, null)
       //GameProcessor.next_player(gameo)
    } else if (actions.contains(ActionNoAction)) {
       action.mtype(MTypeEnum.ACTION_NO_ACTION.toString)
    } else if (actions.contains(ActionEndureAttack)) {
       action.mtype(MTypeEnum.ACTION_ENDUREATTACK.toString)
    } else if (actions.contains(ActionEndureMagic)) {
       action.mtype(MTypeEnum.ACTION_ENDUREMAGIC.toString)
    } else if (roomphase.phase_type.is == RoomPhaseEnum.HEAL_REACTION.toString) {
       action.mtype(MTypeEnum.ACTION_HEAL.toString).action_flags("0").action_flags2(roomphase.last_phase_type.is)
    } else if (roomphase.phase_type.is == RoomPhaseEnum.DISCARD_REACTION.toString) {
       val cards_in_hand = CardPool.in_hand(currentuserentry, gameo.card_list)
       val discard_number = math.max(0, cards_in_hand.length - currentuserentry.get_hand_max)

       val discarded_cards = cards_in_hand.take(discard_number)

       action.mtype(MTypeEnum.ACTION_DISCARD.toString)
             .action_flags(discarded_cards.map(_.to_card.card_enum.toString).mkString(","))
             .action_cards(discarded_cards.map(_.card_no.toString).mkString(","))  
    } else if (roomphase.phase_type.is == RoomPhaseEnum.BACKDISCARD_REACTION.toString) {
       val cards_in_back = CardPool.in_back(currentuserentry, gameo.card_list)
       val discard_number = math.max(0, cards_in_back.length - currentuserentry.get_role.role_back_cards_max)

       val discarded_cards = cards_in_back.take(discard_number)

       action.mtype(MTypeEnum.ACTION_BUTTERFLY_BACKDISCARD.toString)
             .action_flags(discarded_cards.map(_.to_card.card_enum.toString).mkString(","))
             .action_cards(discarded_cards.map(_.card_no.toString).mkString(","))  
    } else if (roomphase.phase_type.is == RoomPhaseEnum.ANGELBLESS_REACTION.toString) {
       val cards_in_hand = CardPool.in_hand(currentuserentry, gameo.card_list)
       val give_number = math.min(cards_in_hand.length, gameo.roomphase.power.is)

       val give_cards = cards_in_hand.take(give_number)

       action.actionee_id(gameo.roomphase.actionee_id.is)
             .mtype(MTypeEnum.ACTION_ANGEL_ANGELBLESS_GIVE.toString)
             .action_flags(give_cards.map(_.to_card.card_enum.toString).mkString(","))
             .action_cards(give_cards.map(_.card_no.toString).mkString(","))  
    } else if (roomphase.phase_type.is == RoomPhaseEnum.AIRRUNE_REACTION.toString) {
       val cards_in_hand = CardPool.in_hand(currentuserentry, gameo.card_list)
       val give_cards    = cards_in_hand(0)

       action.actionee_id(gameo.roomphase.actionee_id.is)
             .mtype(MTypeEnum.ACTION_RUNEMAGE_AIRRUNE_DISCARD.toString)
             .action_card2(give_cards.id.is).action_flags(give_cards.card.is)
                          //.action_flags(give_cards.map(_.to_card.card_enum.toString).mkString(","))
                          //.action_cards(give_cards.map(_.card_no.toString).mkString(","))  
    } else {
       talk.mtype(MTypeEnum.MESSAGE_TIMEOUT.toString)
       talk.save
       talk.send(room)
       gameo.set_next_turn(true)
       GameProcessor.process_main(gameo, null)
       //GameProcessor.next_player(gameo)
    }
    
    if (gameo.is_next_turn == false) {
      talk.save
      talk.send(room)
      RoomActor.process_signal_action(action)
    }
  }
}
