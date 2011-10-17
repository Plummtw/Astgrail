package org.plummtw.astgrail.data

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
import actor._

//import scala.xml.NodeSeq

import org.plummtw.astgrail.model._
import org.plummtw.astgrail.enum._
import org.plummtw.astgrail.actor._
import org.plummtw.astgrail.util._
import org.plummtw.astgrail.card._


//import org.plummtw.shadowhunter.heavy.GameProcessor

trait UserEntryTargetable {
  def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    UserEntry.rrnc(actioner, gameo.userentrys)
  }  
}

trait CardTargetable {
  def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    List()
  }  
}

trait CardTargetable2 {
  def targetable_cards2(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    List()
  }  
}


class ActionData(action: MTypeEnum.Value, str: String) extends Logger  {
  def action_enum     = action
  def tag_string      = str
  def command_name    = "action_" + action.toString
    
  def enabled(gameo : GameObject, actioner : UserEntry) : Boolean= true
  
  def js_command : JsCmd = Noop
  
  def js_dialog(dialog_name : String) = 
    S.runTemplate(List("dialog/" + dialog_name)).
      map(ns => ModalDialog(ns)) openOr
      Alert("Couldn't find " + dialog_name + " template")
      
  def js_action : JsCmd = {
    val gameo = GameO_R.get
    val currentuserentry = CurrentUserEntry_R.get
    
    val roomround = gameo.roomround
    val action = Action.create.roomround_id(gameo.roomround.id.is).actioner_id(currentuserentry.id.is)
                             .mtype(action_enum.toString)
    println("js_action : " + action_enum.toString)
    RoomActor ! SignalAction(gameo.room, action)
    Noop
  }
  
  def toAjaxButton = ajaxButton(this.toString, () => S.callOnce(js_command))

  override def toString(): String = tag_string // "[" + tag_string + "]"
}

class ActionMagic(action: MTypeEnum.Value, str: String) extends ActionData(action, str) {
  /*
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    //val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
    val roomround = gameo.roomround
    (roomround.additional_flag.is == RoomAdditionalFlagEnum.NONE.toString) ||
    (roomround.additional_flag.is == RoomAdditionalFlagEnum.MAGIC.toString) ||
    (roomround.additional_flag.is == RoomAdditionalFlagEnum.BOTH.toString)
  }
  */
}

object ActionNoAction extends ActionData(MTypeEnum.ACTION_NO_ACTION, "不行動") {
  //override def enabled(gameo : GameObject, actioner : UserEntry)  = false
  
  override def js_command : JsCmd = js_action
}

object ActionKick extends ActionData(MTypeEnum.ACTION_KICK, "踢人") with UserEntryTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
    (roomphase.phase_type.is == RoomPhaseEnum.GAMEHALL.toString)
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    val kick_actions = Action.findAll(By(Action.roomround_id, gameo.roomround.id.is),
                                      By(Action.mtype, MTypeEnum.ACTION_KICK.toString),
                                      By(Action.actioner_id, actioner.id.is))
    val kick_actionees = kick_actions.map(_.actionee_id.is)
    
    gameo.userentrys.filter(x=>(x.id.is != actioner.id.is) &&
                               (!x.revoked.is) &&
                               (!kick_actionees.contains(x.id.is)))
  }

  override def js_command : JsCmd = js_dialog("kick_dialog")
}

object ActionStartGame extends ActionData(MTypeEnum.ACTION_STARTGAME, "開始遊戲") {
  override def enabled(gameo : GameObject, actioner : UserEntry) = {
    val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
    
    (roomphase.phase_type.is == RoomPhaseEnum.GAMEHALL.toString) && 
    (actioner.hasnt_room_flag(UserEntryRoomFlagEnum.VOTED))
  }
  
  override def js_command : JsCmd = js_action
}

object ActionAttack extends ActionData(MTypeEnum.ACTION_ATTACK, "攻擊") with UserEntryTargetable with CardTargetable {
  override def js_command : JsCmd = js_dialog("attack_dialog")
  
  //override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    //val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
  //  val roomround = gameo.roomround
  ///  (roomround.additional_flag.is != RoomAdditionalFlagEnum.MAGIC.toString)
  //}
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    val taunting_users = gameo.userentrys.filter(x => (x.get_role == RoleBrave) && (x.target_user.is == actioner.id.is))
    
    val result = 
      if (taunting_users.length != 0)
        taunting_users
      else
        gameo.userentrys.filter(x=>(!x.revoked.is) &&
                                   (x.team_id.is != actioner.team_id.is) &&
                                   ((x.get_role != RoleAssassin) || (!x.tapped.is)))
    
    if ((actioner.get_role == RoleMonk) && (actioner.tapped.is))
      result.filter(_.id.is == actioner.target_user.is)
    else  
      result
  }
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    //val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
    val roomround   = gameo.roomround
        
    val result1 = hand_cards.filter(x => x.to_card.cardtype_enum == CardTypeEnum.ATTACK)
    if (roomround.additional_flag.is == RoomAdditionalFlagEnum.ATTACK_AIR.toString)
      result1.filter(x => x.to_card.cardattr_enum == CardAttrEnum.AIR)
    else if (roomround.additional_flag.is == RoomAdditionalFlagEnum.ATTACK_FIRE.toString)
      result1.filter(x => x.to_card.cardattr_enum == CardAttrEnum.FIRE)
    else      
      result1
    
  }
}

object ActionReattack extends ActionData(MTypeEnum.ACTION_REATTACK, "應戰") with UserEntryTargetable with CardTargetable {
  override def js_command : JsCmd = js_dialog("reattack_dialog")
  
  override def enabled(gameo : GameObject, actioner : UserEntry) = {
    val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
    if (roomphase.card_enum.is == "")
      true
    else {
      val attack_card = CardEnum.get_card(roomphase.card_enum.is)
    
      val is_cant_reattack = {
        val attacker = UserEntry.get(roomphase.actionee_id.is, gameo.userentrys)
        val attacker_role = attacker.get_role
        if (attacker_role == RoleSwordSaint) {
          val shield_in_front = CardPool.in_front_shield(actioner, gameo.card_list)
        
          (attack_card.has_action(MTypeEnum.ACTION_SWORDSAINT_STRONGGALE)) &&
          (shield_in_front.length != 0)
        } else if (attacker_role == RoleArcher) {
          (attack_card.cardattr_enum == CardAttrEnum.THUNDER)
        } else if (attacker_role == RoleAssassin) {
          (attacker.tapped.is)
        } else if (attacker_role == RoleSaintLance) {
          attacker.has_role_flag(UserEntryRoleFlagEnum.SAINTLANCE_NOSMITE)
        } else if (attacker_role == RoleMagicSword) {
          attacker.has_role_flag(UserEntryRoleFlagEnum.MAGICSWORD_DARKSTUN)
        } else if (attacker_role == RoleMonk) {
          (attacker.has_role_flag(UserEntryRoleFlagEnum.MONK_FIRESOUL)) &&
          (roomphase.last_phase_type.is == RoomPhaseEnum.ATTACK.toString)
        } else if (attacker_role == RoleBrave) {
          (attacker.has_role_flag(UserEntryRoleFlagEnum.BRAVE_STILLWATER)) &&
          (roomphase.last_phase_type.is == RoomPhaseEnum.ATTACK.toString)
        } else false
      }  
      
      (attack_card.cardattr_enum != CardAttrEnum.DARK) && (!is_cant_reattack)
    }
  }

  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
    val attacker_id = roomphase.actionee_id.is
    gameo.userentrys.filter(x=>(!x.revoked.is) && (x.id.is != attacker_id) &&
                            (x.team_id.is != actioner.team_id.is))
  }
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    val attack_card = CardEnum.get_card(roomphase.card_enum.is)
    
    hand_cards.filter{x => 
      val card = x.to_card
      (card.cardtype_enum == CardTypeEnum.ATTACK) &&
      ((card.cardattr_enum == attack_card.cardattr_enum) ||
       (card.cardattr_enum == CardAttrEnum.DARK))
    }
  }
}

/* object ActionMagic extends ActionData(MTypeEnum.ACTION_MAGIC, "法術") with UserEntryTargetable with CardTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry) = false
  
  override def js_command : JsCmd = js_dialog("magic_dialog")
} */

object ActionMagicPoison extends ActionMagic(MTypeEnum.ACTION_MAGIC_POISON, "中毒") with UserEntryTargetable with CardTargetable {
  override def js_command : JsCmd = js_dialog("magic_poison_dialog")
  
  override def enabled(gameo : GameObject, actioner : UserEntry)  = 
    (actioner.get_role != RoleMagicSword)
  // val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
  //  (roomphase.additional_flag.is == RoomAdditionalFlagEnum.NONE.toString) ||
  //  (roomphase.additional_flag.is == RoomAdditionalFlagEnum.MAGIC.toString) ||
  //  (roomphase.additional_flag.is == RoomAdditionalFlagEnum.BOTH.toString)
  //}
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    UserEntry.rr(gameo.userentrys)
  }
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.cardmagic_enum == CardMagicEnum.POISON)
  }
}

object ActionMagicWeaken extends ActionMagic(MTypeEnum.ACTION_MAGIC_WEAKEN, "虛弱") with UserEntryTargetable with CardTargetable {
  override def js_command : JsCmd = js_dialog("magic_weaken_dialog")
  
  override def enabled(gameo : GameObject, actioner : UserEntry)  = 
    (actioner.get_role != RoleMagicSword)
  //override def enabled(gameo : GameObject, actioner : UserEntry)  = {
  //  val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
  //  (roomphase.additional_flag.is == RoomAdditionalFlagEnum.NONE.toString) ||
  //  (roomphase.additional_flag.is == RoomAdditionalFlagEnum.MAGIC.toString) ||
  //  (roomphase.additional_flag.is == RoomAdditionalFlagEnum.BOTH.toString)
  //}
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    gameo.userentrys.filter{x => 
      val front_cards  = CardPool.in_front(actioner, gameo.card_list)
      val front_cards_weaken = front_cards.filter(x => x.to_card.cardmagic_enum == CardMagicEnum.WEAKEN)
      (!x.revoked.is) && (front_cards_weaken.length == 0)
    }
  }
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.cardmagic_enum == CardMagicEnum.WEAKEN)
  }
}

object ActionMagicShield extends ActionMagic(MTypeEnum.ACTION_MAGIC_SHIELD, "聖盾") with UserEntryTargetable with CardTargetable {
  override def js_command : JsCmd = js_dialog("magic_shield_dialog")
  
  override def enabled(gameo : GameObject, actioner : UserEntry)  = 
    (actioner.get_role != RoleMagicSword)
  //override def enabled(gameo : GameObject, actioner : UserEntry)  = {
  //  val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
  //  (roomphase.additional_flag.is == RoomAdditionalFlagEnum.NONE.toString) ||
  //  (roomphase.additional_flag.is == RoomAdditionalFlagEnum.MAGIC.toString) ||
  //  (roomphase.additional_flag.is == RoomAdditionalFlagEnum.BOTH.toString)
  //}
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    gameo.userentrys.filter{x => 
      val front_cards  = CardPool.in_front(x, gameo.card_list)
      val front_cards_shield = front_cards.filter{y => 
        val carddata = y.to_card
        (carddata.cardmagic_enum == CardMagicEnum.SHIELD) ||
        (carddata.has_action(MTypeEnum.ACTION_ANGEL_ANGELICWALL))}
      (!x.revoked.is) && (front_cards_shield.length == 0)
    }
  }
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    val is_angel = actioner.get_role == RoleAngel
    hand_cards.filter{ x =>
      val carddata = x.to_card
      (carddata.cardmagic_enum == CardMagicEnum.SHIELD) ||
      ((is_angel) &&  (carddata.has_action(MTypeEnum.ACTION_ANGEL_ANGELICWALL)))
    }
  }
}

object ActionMagicMBolt extends ActionMagic(MTypeEnum.ACTION_MAGIC_MBOLT, "魔彈") with UserEntryTargetable with CardTargetable {
  override def js_command : JsCmd = js_dialog("magic_mbolt_dialog")

  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
    (roomphase.phase_type.is == RoomPhaseEnum.MBOLT_REACTION.toString) ||
    (actioner.get_role != RoleMagicSword)  
  }
  //override def enabled(gameo : GameObject, actioner : UserEntry)  = {
  //  val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
  //  (roomphase.additional_flag.is == RoomAdditionalFlagEnum.NONE.toString) ||
  //  (roomphase.additional_flag.is == RoomAdditionalFlagEnum.MAGIC.toString) ||
  //  (roomphase.additional_flag.is == RoomAdditionalFlagEnum.BOTH.toString)
  //}
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    val userentrys_rr = UserEntry.rr(gameo.userentrys)
    val player_index = userentrys_rr.indexOf(actioner)
    var index  = player_index
    var index2 = player_index
    
    val is_reversed = gameo.roomround.has_flag(RoomRoundFlagEnum.MBOLT_REVERSE)
    
    val mbolt_userentrys = userentrys_rr.filter(_.has_user_flag(UserEntryFlagEnum.MBOLT))
    val is_all_mbolted = (mbolt_userentrys.length == userentrys_rr.length)
    
    var do_guard = 0
    do {
      index = index - 1
      if (index < 0)
        index = userentrys_rr.length - 1
      do_guard = do_guard + 1
    } while((do_guard < 100) &&
            ((userentrys_rr(index).team_id == actioner.team_id) ||
            ((!is_all_mbolted) && (userentrys_rr(index).has_user_flag(UserEntryFlagEnum.MBOLT)))))
   
    do_guard = 0
    do {
      index2 = index2 + 1
      if (index2 >= userentrys_rr.length )
        index2 = 0
      do_guard = do_guard + 1
    } while((do_guard < 100) &&
            ((userentrys_rr(index2).team_id == actioner.team_id) ||
            ((!is_all_mbolted) && (userentrys_rr(index2).has_user_flag(UserEntryFlagEnum.MBOLT)))))
    
    if (is_reversed) {
      warn("MBOLT_REVERSE DEBUG START")
      warn("userentrys : " + gameo.userentrys)
      warn("actioner : "  + actioner)
      warn("player_index : " + player_index)
      warn("index : "+ index)
      warn("index2 : "+ index2)
      warn("MBOLT_REVERSE DEBUG END")
    }
    
    if (index == index2)
      List(userentrys_rr(index))
    else if ((actioner.get_role == RoleMage) &&
             (gameo.roomphase.phase_type.is != RoomPhaseEnum.MBOLT_REACTION.toString))
      List(userentrys_rr(index2), userentrys_rr(index))
    else if (is_reversed)
      List(userentrys_rr(index))
    else
      List(userentrys_rr(index2))
  }
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    if (actioner.get_role == RoleMage)
      hand_cards.filter{ x =>
        val card = x.to_card
        (card.cardmagic_enum == CardMagicEnum.MBOLT) ||
        (card.cardattr_enum == CardAttrEnum.FIRE) ||
        (card.cardattr_enum == CardAttrEnum.EARTH)
      }  
    else
      hand_cards.filter(x => x.to_card.cardmagic_enum == CardMagicEnum.MBOLT)
  }
}


object ActionPurchase extends ActionData(MTypeEnum.ACTION_PURCHASE, "購買") {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    val userentryteam = UserEntryTeam.get(actioner.team_id, gameo.userentryteams)
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    //(userentryteam.gems.is + userentryteam.crystals.is < 5) &&
    (hand_cards.length + 3 <= actioner.get_hand_max)
  }
  
  override def js_command : JsCmd = js_action
}

object ActionCombine extends ActionData(MTypeEnum.ACTION_COMBINE, "合成") {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    val userentryteam = UserEntryTeam.get(actioner.team_id, gameo.userentryteams)
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    (userentryteam.gems.is + userentryteam.crystals.is >= 3) &&
    (hand_cards.length + 3 <= actioner.get_hand_max)
  }
  
  override def js_command : JsCmd = js_dialog("combine_dialog")
}

object ActionRefine extends ActionData(MTypeEnum.ACTION_REFINE, "提煉") {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    val userentryteam = UserEntryTeam.get(actioner.team_id, gameo.userentryteams)
    //val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    val role = actioner.get_role
    
    if (role.role_is_gem_only)
      (userentryteam.gems.is >= 1) &&
      (actioner.gems.is + actioner.crystals.is < actioner.energy_max)
    else  
      (userentryteam.gems.is + userentryteam.crystals.is >= 1) &&
      (actioner.gems.is + actioner.crystals.is < actioner.energy_max)
  }
  
  override def js_command : JsCmd = js_dialog("refine_dialog")
}

object ActionDiscard extends ActionData(MTypeEnum.ACTION_DISCARD, "棄牌") {
  //override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    //val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    //(hand_cards.length > actioner.get_hand_max)
  //}
  
  override def js_command : JsCmd = js_dialog("discard_dialog")
}

object ActionCardRenew extends ActionData(MTypeEnum.ACTION_CARD_RENEW, "重洗手牌") {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    true
  }
  
  override def js_command : JsCmd = {
    val gameo = GameO_R.get
    val currentuserentry = CurrentUserEntry_R.get
    
    val roomround = gameo.roomround
    val hand_cards  = CardPool.in_hand(currentuserentry, gameo.card_list)
    
    val action = Action.create.roomround_id(gameo.roomround.id.is).actioner_id(currentuserentry.id.is)
                       .mtype(action_enum.toString).action_flags(hand_cards.map(_.to_card.card_enum.toString).mkString(","))
                       .action_cards(hand_cards.map(_.card_no.toString).mkString(","))                       
    println("js_action : " + action_enum.toString)
    RoomActor ! SignalAction(gameo.room, action)
    Noop
  }
}

object ActionLight extends ActionData(MTypeEnum.ACTION_LIGHT, "聖光") with CardTargetable {
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    
    //if (actioner.get_role == RoleAngel) {
    //  hand_cards.filter{ hand_card =>
    //    val card = hand_card.to_card
    //    (card.cardmagic_enum == CardMagicEnum.LIGHT) ||
    //    (card.has_action(MTypeEnum.ACTION_ANGEL_ANGELICWALL))
    //  }  
    //} else
    hand_cards.filter(_.to_card.cardmagic_enum == CardMagicEnum.LIGHT)
  }
  
  override def js_command : JsCmd = js_dialog("light_dialog")
}

object ActionHeal extends ActionData(MTypeEnum.ACTION_HEAL, "治療")  {
  //override def enabled(gameo : GameObject, actioner : UserEntry)  = {
  //  (actioner.heals.is > 0)
  //}
  
  override def js_command : JsCmd = js_dialog("heal_dialog")
}

object ActionEndureAttack extends ActionData(MTypeEnum.ACTION_ENDUREATTACK, "承受攻擊") {
  override def js_command : JsCmd = js_action
}

object ActionEndureMagic extends ActionData(MTypeEnum.ACTION_ENDUREMAGIC, "承受魔法") {
  override def js_command : JsCmd = js_action 
  //{
  //  val gameo = GameO_R.get
  //  val currentuserentry = CurrentUserEntry_R.get
  //  
  //  val roomround = gameo.roomround
  //  val roomphase = gameo.roomphase
  //  val action = Action.create.roomround_id(gameo.roomround.id.is).actioner_id(currentuserentry.id.is)
  //                           .mtype(action_enum.toString) //.action_flags(roomphase.phase_type.is)
  //  println("js_action : " + action_enum.toString)
  //  RoomActor ! SignalAction(action)
  //  Noop
  //}
}

object ActionSkipTurn extends ActionData(MTypeEnum.ACTION_SKIPTURN, "跳過回合") {
  override def js_command : JsCmd = js_action
}


object ActionSwordSaintCombo extends ActionData(MTypeEnum.ACTION_SWORDSAINT_COMBO, "連續技") {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    val cards_in_hand = CardPool.in_hand(actioner, gameo.card_list)
    val roomphase = gameo.roomphase
    
    //((roomphase.phase_type.is == RoomPhaseEnum.ATTACK.toString) ||
    // (roomphase.phase_type.is == RoomPhaseEnum.MAIN.toString)) &&
    (actioner.hasnt_role_flag(UserEntryRoleFlagEnum.SWORDSAINT_COMBO)) &&
    (cards_in_hand.map(_.to_card).filter(x =>
        (x.cardattr_enum == CardAttrEnum.AIR) &&
        (x.cardtype_enum == CardTypeEnum.ATTACK)).length != 0)
  }
  
  override def js_command : JsCmd = js_action
}

object ActionSwordSaintSwordShadow extends ActionData(MTypeEnum.ACTION_SWORDSAINT_SWORDSHADOW, "劍影") {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    val cards_in_hand = CardPool.in_hand(actioner, gameo.card_list)
    val roomphase = gameo.roomphase
    
    //((roomphase.phase_type.is == RoomPhaseEnum.ATTACK.toString) ||
    // (roomphase.phase_type.is == RoomPhaseEnum.MAIN.toString)) &&
    (actioner.hasnt_role_flag(UserEntryRoleFlagEnum.SWORDSAINT_SWORDSHADOW)) &&
    (actioner.gems.is + actioner.crystals.is > 0)
  }
  
  override def js_command : JsCmd = js_action
}

object ActionBerserkerLacerate extends ActionData(MTypeEnum.ACTION_BERSERKER_LACERATE, "撕裂") {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is > 0)
  }
  
  override def js_command : JsCmd = js_action
}

object ActionArcherTrap extends ActionMagic(MTypeEnum.ACTION_ARCHER_TRAP, "閃光陷阱") with UserEntryTargetable with CardTargetable {
  override def js_command : JsCmd = js_dialog("archer_trap_dialog")
  
  //override def enabled(gameo : GameObject, actioner : UserEntry)  = {
  //  val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
  //  (roomphase.additional_flag.is == RoomAdditionalFlagEnum.NONE.toString) ||
  //  (roomphase.additional_flag.is == RoomAdditionalFlagEnum.MAGIC.toString) ||
  //  (roomphase.additional_flag.is == RoomAdditionalFlagEnum.BOTH.toString)
  //}
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    UserEntry.rr(gameo.userentrys)
  }
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.has_action(MTypeEnum.ACTION_ARCHER_TRAP))
  }
}

object ActionArcherSnipe extends ActionMagic(MTypeEnum.ACTION_ARCHER_SNIPE, "狙擊") with UserEntryTargetable {
  override def js_command : JsCmd = js_dialog("archer_snipe_dialog")
  
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    //val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
    //val roomround = gameo.roomround
    //((roomround.additional_flag.is == RoomAdditionalFlagEnum.NONE.toString) ||
    // (roomround.additional_flag.is == RoomAdditionalFlagEnum.MAGIC.toString) ||
    // (roomround.additional_flag.is == RoomAdditionalFlagEnum.BOTH.toString)) &&
    (actioner.gems.is + actioner.crystals.is > 0)
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    UserEntry.rr(gameo.userentrys)
  }
}

object ActionArcherPenetrate extends ActionMagic(MTypeEnum.ACTION_ARCHER_PENETRATE, "貫穿射擊") with CardTargetable {
  override def js_command : JsCmd = js_dialog("archer_penetrate_dialog")
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.cardtype_enum == CardTypeEnum.MAGIC)
  }
}

object ActionAssassinWaterShadow extends ActionMagic(MTypeEnum.ACTION_ASSASSIN_WATERSHADOW, "水影") with CardTargetable {
  override def js_command : JsCmd = js_dialog("assassin_watershadow_dialog")
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.cardattr_enum == CardAttrEnum.WATER)
  }
}
object ActionAssassinSneak extends ActionMagic(MTypeEnum.ACTION_ASSASSIN_SNEAK, "潛行") {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is > 0)
  }
  
  override def js_command : JsCmd = js_action
}
object ActionAngelWindPurify extends ActionMagic(MTypeEnum.ACTION_ANGEL_WINDPURIFY, "風之潔淨") with CardTargetable with CardTargetable2 {
  override def js_command : JsCmd = js_dialog("angel_windpurify_dialog")
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.cardattr_enum == CardAttrEnum.AIR)
  }
  
  override def targetable_cards2(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    gameo.card_list.filter(_.position.is == CardPositionEnum.FRONT.toString)
  }
}
object ActionAngelAngelBless extends ActionMagic(MTypeEnum.ACTION_ANGEL_ANGELBLESS, "天使祝福") with UserEntryTargetable with CardTargetable {
  override def js_command : JsCmd = js_dialog("angel_angelbless_dialog")
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.cardattr_enum == CardAttrEnum.WATER)
  }
}

object ActionAngelAngelBlessGive extends ActionMagic(MTypeEnum.ACTION_ANGEL_ANGELBLESS_GIVE, "天使祝福給予") with CardTargetable {
  override def js_command : JsCmd = js_dialog("angel_angelbless_give_dialog")
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards
  }
}

//object ActionAngelAngelBond extends ActionMagic(MTypeEnum.ACTION_ANGEL_ANGELBOND, "天使羈絆")

object ActionAngelAngelSong extends ActionMagic(MTypeEnum.ACTION_ANGEL_ANGELSONG, "天使之歌") with CardTargetable {
  override def js_command : JsCmd = js_dialog("angel_angelsong_dialog")
  
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is + actioner.crystals.is > 0)
  }
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    gameo.card_list.filter(_.position.is == CardPositionEnum.FRONT.toString)
  }
}

object ActionAngelGodCover extends ActionMagic(MTypeEnum.ACTION_ANGEL_GODCOVER, "神之庇護") {
  override def js_command : JsCmd = js_dialog("angel_godcover_dialog")
  
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is + actioner.crystals.is > 0)
  }
}

object ActionSaintGirlHeal extends ActionMagic(MTypeEnum.ACTION_SAINTGIRL_HEAL, "治療術") with UserEntryTargetable with CardTargetable {
  override def js_command : JsCmd = js_dialog("saintgirl_heal_dialog")
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    UserEntry.rr(gameo.userentrys)
  }
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.has_action(MTypeEnum.ACTION_SAINTGIRL_HEAL))
  }
}
object ActionSaintGirlHealLight extends ActionMagic(MTypeEnum.ACTION_SAINTGIRL_HEALLIGHT, "治療之光") with UserEntryTargetable with CardTargetable {
  override def js_command : JsCmd = js_dialog("saintgirl_heallight_dialog")
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    UserEntry.rr(gameo.userentrys)
  }
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.has_action(MTypeEnum.ACTION_SAINTGIRL_HEALLIGHT))
  }
}

object ActionSaintGirlCompassion extends ActionMagic(MTypeEnum.ACTION_SAINTGIRL_COMPASSION, "憐憫") {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is > 0) && (!actioner.tapped.is)
  }
  
  override def js_command : JsCmd = js_action
}

object ActionSaintGirlHolyHeal extends ActionMagic(MTypeEnum.ACTION_SAINTGIRL_HOLYHEAL, "聖療") with UserEntryTargetable {
  override def js_command : JsCmd = js_dialog("saintgirl_holyheal_dialog")
  
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is + actioner.crystals.is > 0) && (actioner.hasnt_role_flag(UserEntryRoleFlagEnum.SAINTGIRL_HOLYHEAL))
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    UserEntry.rr(gameo.userentrys)
  }
}

object ActionSealerSeal extends ActionMagic(MTypeEnum.ACTION_SEALER_SEAL, "封印") with UserEntryTargetable with CardTargetable {
  override def js_command : JsCmd = js_dialog("sealer_seal_dialog")
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    UserEntry.rr(gameo.userentrys)
  }
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.has_action(MTypeEnum.ACTION_SEALER_SEAL))
  }
}
object ActionSealerFiveSeal extends ActionMagic(MTypeEnum.ACTION_SEALER_FIVESEAL, "五系束縛") with UserEntryTargetable {
  override def js_command : JsCmd = js_dialog("sealer_fiveseal_dialog")
  
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is + actioner.crystals.is > 0)
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    UserEntry.rr(gameo.userentrys)
  }
}
object ActionSealerBreakSeal extends ActionMagic(MTypeEnum.ACTION_SEALER_BREAKSEAL, "封印破碎") with CardTargetable {
  override def js_command : JsCmd = js_dialog("sealer_breakseal_dialog")
  
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is + actioner.crystals.is > 0)
  }
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    gameo.card_list.filter(_.position.is == CardPositionEnum.FRONT.toString)
  }
}
object ActionMageShock extends ActionMagic(MTypeEnum.ACTION_MAGE_SHOCK, "魔爆衝擊") with UserEntryTargetable with CardTargetable {
  override def js_command : JsCmd = js_dialog("mage_shock_dialog")
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    gameo.userentrys.filter(x=>(!x.revoked.is) &&
                               (x.team_id.is != actioner.team_id.is))
  }
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.cardtype_enum == CardTypeEnum.MAGIC)
  }
}

object ActionMageShockDiscard extends ActionMagic(MTypeEnum.ACTION_MAGE_SHOCK_DISCARD, "魔爆衝擊棄牌") with CardTargetable {
  override def js_command : JsCmd = js_dialog("mage_shock_discard_dialog")
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.cardtype_enum == CardTypeEnum.MAGIC)
  }
}

object ActionMageStorm extends ActionMagic(MTypeEnum.ACTION_MAGE_STORM, "毀滅風暴") with UserEntryTargetable {
  override def js_command : JsCmd = js_dialog("mage_storm_dialog")
  
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is > 0)
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    gameo.userentrys.filter(x=>(!x.revoked.is) &&
                               (x.team_id.is != actioner.team_id.is))
  }
}

object ActionSaintLanceShine extends ActionData(MTypeEnum.ACTION_SAINTLANCE_SHINE, "輝耀") with CardTargetable {
  override def js_command : JsCmd = js_dialog("saintlance_shine_dialog")
  
    override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.cardattr_enum == CardAttrEnum.WATER)
  }
}

object ActionSaintLanceSmite extends ActionData(MTypeEnum.ACTION_SAINTLANCE_SMITE, "聖擊") {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    actioner.hasnt_role_flag(UserEntryRoleFlagEnum.SAINTLANCE_NOSMITE)
  }
  
  override def js_command : JsCmd = js_action
}

object ActionSaintLanceRetribution extends ActionData(MTypeEnum.ACTION_SAINTLANCE_RETRIBUTION, "懲戒") with UserEntryTargetable with CardTargetable {
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    gameo.userentrys.filter(x=>(!x.revoked.is) && (x.id.is != actioner.id.is) &&
                               (x.heals.is > 0))
  }
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.cardtype_enum == CardTypeEnum.MAGIC)
  }
  
  override def js_command : JsCmd = js_dialog("saintlance_retribution_dialog")
}
//object ActionSaintLanceSkyLance
object ActionSaintLanceEarthLance extends ActionData(MTypeEnum.ACTION_SAINTLANCE_EARTHLANCE, "地槍") {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    val roomphase = gameo.roomphase
    (actioner.heals.is > 0) && (roomphase.last_phase_type.is == RoomPhaseEnum.ATTACK.toString)
  }
  
  override def js_command : JsCmd = js_dialog("saintlance_earthlance_dialog")
}
object ActionSaintLanceHolyPray extends ActionData(MTypeEnum.ACTION_SAINTLANCE_HOLYPRAY, "聖光祈癒") {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is > 0)
  }
  
  override def js_command : JsCmd = js_action
}

object ActionElementalistMagic extends ActionData(MTypeEnum.ACTION_ELEMENTALIST_MAGIC, "元素魔法") with UserEntryTargetable with CardTargetable{
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    UserEntry.rr(gameo.userentrys)
  }
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.has_action(MTypeEnum.ACTION_ELEMENTALIST_MAGIC))
  }
  
  override def js_command : JsCmd = js_dialog("elementalist_magic_dialog")
}

object ActionElementalistIgnite extends ActionData(MTypeEnum.ACTION_ELEMENTALIST_IGNITE, "元素點燃") with UserEntryTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.yellow_index.is >= 3 )
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    UserEntry.rr(gameo.userentrys)
  }
  
  override def js_command : JsCmd = js_dialog("elementalist_ignite_dialog")
}

object ActionElementalistMoonLight extends ActionData(MTypeEnum.ACTION_ELEMENTALIST_MOONLIGHT, "月光") with UserEntryTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is > 0)
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    UserEntry.rr(gameo.userentrys)
  }
  
  override def js_command : JsCmd = js_dialog("elementalist_moonlight_dialog")
}

object ActionNecromancerPlague extends ActionData(MTypeEnum.ACTION_NECROMANCER_PLAGUE, "瘟疫") with CardTargetable {
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.cardattr_enum == CardAttrEnum.EARTH)
  }
  
  override def js_command : JsCmd = js_dialog("necromancer_plague_dialog")
}

object ActionNecromancerDeathTouch   extends ActionData(MTypeEnum.ACTION_NECROMANCER_DEATHTOUCH, "死亡之觸") with UserEntryTargetable with CardTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.heals.is >= 2)
  }
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards
  }
  
  override def js_command : JsCmd = js_dialog("necromancer_deathtouch_dialog")
}

object ActionNecromancerGraveFall extends ActionData(MTypeEnum.ACTION_NECROMANCER_GRAVEFALL, "墓碑隕落")  {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is > 0)
  }
  
  override def js_command : JsCmd = js_action
}  

object ActionMagicSwordGather extends ActionData(MTypeEnum.ACTION_MAGICSWORD_GATHER, "暗影凝聚")  {
  override def js_command : JsCmd = js_action
}

object ActionMagicSwordComet extends ActionData(MTypeEnum.ACTION_MAGICSWORD_COMET, "暗影流星") with UserEntryTargetable with CardTargetable  {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.tapped.is) && (targetable_cards(gameo, actioner).length >= 2)
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    UserEntry.rr(gameo.userentrys)
  }
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.cardtype_enum == CardTypeEnum.MAGIC)
  }
  
  override def js_command : JsCmd = js_dialog("magicsword_comet_dialog")
}

/*object ActionMagicSwordCometNoCheckTap extends ActionData(MTypeEnum.ACTION_MAGICSWORD_COMET, "暗影流星") with UserEntryTargetable with CardTargetable  {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (targetable_cards(gameo, actioner).length >= 2)
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    UserEntry.rr(gameo.userentrys)
  }
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.cardtype_enum == CardTypeEnum.MAGIC)
  }
  
  override def js_command : JsCmd = js_dialog("magicsword_comet_dialog")
}*/

object ActionJudicatorBreakRitual extends ActionData(MTypeEnum.ACTION_JUDICATOR_BREAKRITUAL, "儀式中斷")  {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.tapped.is)
  }
  
  override def js_command : JsCmd = js_action
}

object ActionJudicatorFinalJudge extends ActionData(MTypeEnum.ACTION_JUDICATOR_FINALJUDGE, "末日審判") with UserEntryTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.yellow_index.is > 0)
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    UserEntry.rr(gameo.userentrys)
  }
  
  override def js_command : JsCmd = js_dialog("judicator_finaljudge_dialog")
}

object ActionJudicatorRitual extends ActionData(MTypeEnum.ACTION_JUDICATOR_RITUAL, "仲裁儀式") {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is > 0) && (!actioner.tapped.is)
  }
  
  override def js_command : JsCmd = js_action
}

object ActionJudicatorBalance extends ActionData(MTypeEnum.ACTION_JUDICATOR_BALANCE, "判決天平")  {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is + actioner.crystals.is > 0)
  }
  
  override def js_command : JsCmd = js_dialog("judicator_balance_dialog")
}

object ActionAdventurerDeceive extends ActionData(MTypeEnum.ACTION_ADVENTURER_DECEIVE, "欺詐") with UserEntryTargetable {
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = 
    ActionAttack.targetable_users(gameo, actioner)
  
  override def js_command : JsCmd = js_dialog("adventurer_deceive_dialog")
}

object ActionAdventurerAddOn extends ActionData(MTypeEnum.ACTION_ADVENTURER_ADDON, "特殊加工")  {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.crystals.is > 0) && (actioner.hasnt_role_flag(UserEntryRoleFlagEnum.ADVENTURER_ADDON))
  }
  
  override def js_command : JsCmd = js_action
}

object ActionAdventurerTheft extends ActionData(MTypeEnum.ACTION_ADVENTURER_THEFT, "偷天換日")  {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    val userentryteam = UserEntryTeam.get(actioner.team_id.is, gameo.userentryteams)
    val enemyteam = gameo.userentryteams.filterNot(_ == userentryteam)(0)
    
    (actioner.gems.is + actioner.crystals.is > 0) && (enemyteam.gems.is > 0) && (actioner.hasnt_role_flag(UserEntryRoleFlagEnum.ADVENTURER_THEFT))
  }
  
  override def js_command : JsCmd = js_action
}


object ActionBishopHolyBless extends ActionData(MTypeEnum.ACTION_BISHOP_HOLYBLESS, "神聖祈福") {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    (hand_cards.filter(x => x.to_card.cardtype_enum == CardTypeEnum.MAGIC).length >= 2)
  }
  
  override def js_command : JsCmd = js_dialog("bishop_holybless_dialog")
}

object ActionBishopHolyWater extends ActionData(MTypeEnum.ACTION_BISHOP_HOLYWATER, "水之神力") with UserEntryTargetable with CardTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    (hand_cards.length >= 2)
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    gameo.userentrys.filter(x=>(!x.revoked.is) &&
                               (x.team_id.is == actioner.team_id.is) &&
                               (x.id.is != actioner.id.is))
  }
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.cardattr_enum == CardAttrEnum.WATER)
  }
  
  override def js_command : JsCmd = js_dialog("bishop_holywater_dialog")
}

object ActionBishopHolyContract extends ActionData(MTypeEnum.ACTION_BISHOP_HOLYCONTRACT, "神聖契約") with UserEntryTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is + actioner.crystals.is > 0) //&& (actioner.heals.is  > 0)
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    gameo.userentrys.filter(x=>(!x.revoked.is) &&
                               (x.team_id.is == actioner.team_id.is) &&
                               (x.id.is != actioner.id.is))
  }
  
  override def js_command : JsCmd = js_dialog("bishop_holycontract_dialog")
}

object ActionBishopHolyField extends ActionData(MTypeEnum.ACTION_BISHOP_HOLYFIELD, "神聖領域") with UserEntryTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    (actioner.gems.is + actioner.crystals.is > 0) && (hand_cards.length >= 2)
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    gameo.userentrys.filter(x=>(!x.revoked.is) &&
                               (x.id.is != actioner.id.is))
  }
  
  override def js_command : JsCmd = js_dialog("bishop_holyfield_dialog")
}

object ActionSageReflect extends ActionData(MTypeEnum.ACTION_SAGE_REFLECT, "法術反彈") with UserEntryTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    (hand_cards.length >= 2)
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    UserEntry.rr(gameo.userentrys)  
  }
  
  override def js_command : JsCmd = js_dialog("sage_reflect_dialog")
}

object ActionSageMagicBook extends ActionData(MTypeEnum.ACTION_SAGE_MAGICBOOK, "魔道法典") with UserEntryTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    (actioner.gems.is > 0) && (hand_cards.length >= 2)
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    UserEntry.rr(gameo.userentrys)  
  }
  
  override def js_command : JsCmd = js_dialog("sage_magicbook_dialog")
}

object ActionSageHolyBook extends ActionData(MTypeEnum.ACTION_SAGE_HOLYBOOK, "聖潔法典") with UserEntryTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    (actioner.gems.is > 0) && (hand_cards.length >= 3)
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    UserEntry.rr(gameo.userentrys)  
  }
  
  override def js_command : JsCmd = js_dialog("sage_holybook_dialog")
}

object ActionRuneMageThunderRune extends ActionData(MTypeEnum.ACTION_RUNEMAGE_THUNDERRUNE, "靈符雷鳴") with UserEntryTargetable with CardTargetable {
  //override def enabled(gameo : GameObject, actioner : UserEntry)  = {
  //  val back_cards  = CardPool.in_back(actioner, gameo.card_list)
  //  (back_cards.length < 2)
  //}
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    UserEntry.rr(gameo.userentrys)  
  }
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.cardattr_enum == CardAttrEnum.THUNDER)
  }
  
  override def js_command : JsCmd = js_dialog("runemage_thunderrune_dialog")
}
object ActionRuneMageAirRune extends ActionData(MTypeEnum.ACTION_RUNEMAGE_AIRRUNE, "靈符風行") with UserEntryTargetable with CardTargetable {
  //override def enabled(gameo : GameObject, actioner : UserEntry)  = {
  //  val back_cards  = CardPool.in_back(actioner, gameo.card_list)
  //  (back_cards.length < 2)
  //}
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    UserEntry.rr(gameo.userentrys)  
  }
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.cardattr_enum == CardAttrEnum.AIR)
  }
  
  override def js_command : JsCmd = js_dialog("runemage_airrune_dialog")
}
object ActionRuneMageAirRuneDiscard extends ActionMagic(MTypeEnum.ACTION_RUNEMAGE_AIRRUNE_DISCARD, "靈符風行棄牌") with CardTargetable {
  override def js_command : JsCmd = js_dialog("runemage_airrune_discard_dialog")
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards
  }
}
object ActionRuneMage100Ghosts extends ActionData(MTypeEnum.ACTION_RUNEMAGE_100GHOSTS, "百鬼夜行") with UserEntryTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    val back_cards  = CardPool.in_back(actioner, gameo.card_list)
    (back_cards.length > 0)
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    UserEntry.rr(gameo.userentrys)  
  }
  
  override def js_command : JsCmd = js_dialog("runemage_100ghosts_dialog")
}

object ActionPrayerPowerBless extends ActionData(MTypeEnum.ACTION_PRAYER_POWERBLESS, "威力賜福") with UserEntryTargetable with CardTargetable {
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    gameo.userentrys.filter(x=>(!x.revoked.is) && (x.id.is != actioner.id.is) &&
                               (x.team_id.is == actioner.team_id.is) &&
                               (CardPool.in_front(x, gameo.card_list)
                                .filter(_.to_card.has_action(MTypeEnum.ACTION_PRAYER_POWERBLESS)).length == 0))
  }
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.has_action(MTypeEnum.ACTION_PRAYER_POWERBLESS))
  }
  
  override def js_command : JsCmd = js_dialog("prayer_powerbless_dialog")
}

object ActionPrayerPowerBlessUse extends ActionData(MTypeEnum.ACTION_PRAYER_POWERBLESS_USE, "使用威力賜福") {
  override def js_command : JsCmd = js_action
}

object ActionPrayerFastBless extends ActionData(MTypeEnum.ACTION_PRAYER_FASTBLESS, "迅捷賜福") with UserEntryTargetable with CardTargetable{
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    gameo.userentrys.filter(x=>(!x.revoked.is) && (x.id.is != actioner.id.is) &&
                               (x.team_id.is == actioner.team_id.is) &&
                               (CardPool.in_front(x, gameo.card_list)
                                .filter(_.to_card.has_action(MTypeEnum.ACTION_PRAYER_FASTBLESS)).length == 0))
  }
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.has_action(MTypeEnum.ACTION_PRAYER_FASTBLESS))
  }
  
  override def js_command : JsCmd = js_dialog("prayer_fastbless_dialog")
}

object ActionPrayerFastBlessUse extends ActionData(MTypeEnum.ACTION_PRAYER_FASTBLESS_USE, "使用迅捷賜福") {
  override def js_command : JsCmd = js_action
}

object ActionPrayerShineBelieve extends ActionData(MTypeEnum.ACTION_PRAYER_SHINEBELIEVE, "光輝信仰") with UserEntryTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.tapped.is) && (actioner.yellow_index.is > 0 )  
    // (CardPool.in_hand(actioner, gameo.card_list).length >= 2)
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    gameo.userentrys.filter(x=>(!x.revoked.is) && (x.id.is != actioner.id.is) &&
                               (x.team_id.is == actioner.team_id.is))
  }
  
  override def js_command : JsCmd = js_dialog("prayer_shinebelieve_dialog")
}

object ActionPrayerDarkBelieve extends ActionData(MTypeEnum.ACTION_PRAYER_DARKBELIEVE, "漆黑信仰") with UserEntryTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.tapped.is) && (actioner.yellow_index.is > 0 ) 
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    gameo.userentrys.filter(x=>(!x.revoked.is) && (x.id.is != actioner.id.is))
  }
  
  override def js_command : JsCmd = js_dialog("prayer_darkbelieve_dialog")
}

object ActionPrayerPray extends ActionData(MTypeEnum.ACTION_PRAYER_PRAY, "祈禱")  {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is > 0) && (!actioner.tapped.is)
  }
  
  override def js_command : JsCmd = js_action
}

object ActionPrayerManaTide extends ActionData(MTypeEnum.ACTION_PRAYER_MANATIDE, "法力潮汐")  {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is + actioner.crystals.is > 0) && (actioner.hasnt_role_flag(UserEntryRoleFlagEnum.PRAYER_MANATIDE))
  }
  
  override def js_command : JsCmd = js_action
}

object ActionSwordEmpSwordKi extends ActionData(MTypeEnum.ACTION_SWORDEMP_SWORDKI, "劍氣斬") with UserEntryTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.yellow_index.is > 0) 
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    val roomphase = gameo.roomphase
    gameo.userentrys.filter(x => (!x.revoked.is) && (x.id.is != roomphase.actionee_id.is))  
  }
  
  override def js_command : JsCmd = js_dialog("swordemp_swordki_dialog")
}

object ActionSwordEmpLibidinalWill extends ActionData(MTypeEnum.ACTION_SWORDEMP_LIBIDINALWILL, "不屈意志")  {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is + actioner.crystals.is > 0) && (actioner.hasnt_role_flag(UserEntryRoleFlagEnum.PRAYER_MANATIDE))
  }
  
  override def js_command : JsCmd = js_action
}

object ActionMonkKiShot extends ActionData(MTypeEnum.ACTION_MONK_KISHOT, "念彈")  with UserEntryTargetable {
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    //val roomphase = gameo.roomphase
    gameo.userentrys.filter(x => (!x.revoked.is) && (x.team_id.is != actioner.team_id.is))  
  }
  
  override def js_command : JsCmd = js_dialog("monk_kishot_dialog")
}

object ActionMonk100Dragons extends ActionData(MTypeEnum.ACTION_MONK_100DRAGONS, "百式幻龍拳") with UserEntryTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.yellow_index.is >= 3) && (!actioner.tapped.is)
  }

  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = 
    UserEntry.rr(gameo.userentrys)
  
  override def js_command : JsCmd = js_dialog("monk_100dragons_dialog")
}

object ActionMonk100DragonsRemove extends ActionData(MTypeEnum.ACTION_MONK_100DRAGONS_REMOVE, "取消百式幻龍拳")  {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.tapped.is)
  }

  override def js_command : JsCmd = js_action
}

object ActionMonkMonkGod extends ActionData(MTypeEnum.ACTION_MONK_MONKGOD, "鬥神天驅")  {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is + actioner.crystals.is > 0) && 
    (CardPool.in_hand(actioner, gameo.card_list).length >= 3)
  }
  
  override def js_command : JsCmd = js_dialog("monk_monkgod_dialog")
}

object ActionBraveTaunt extends ActionData(MTypeEnum.ACTION_BRAVE_TAUNT, "挑釁") with UserEntryTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.yellow_index.is > 0) 
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    //val roomphase = gameo.roomphase
    gameo.userentrys.filter(x => (!x.revoked.is) && (x.team_id.is != actioner.team_id.is))  
  }
  
  override def js_command : JsCmd = js_dialog("brave_taunt_dialog")
}

object ActionBraveForbidden extends ActionData(MTypeEnum.ACTION_BRAVE_FORBIDDEN, "禁斷之力")  {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is + actioner.crystals.is > 0)
  }
  
  override def js_command : JsCmd = {
    val gameo = GameO_R.get
    val currentuserentry = CurrentUserEntry_R.get
    
    val roomround = gameo.roomround
    val cards = CardPool.in_hand(currentuserentry, gameo.card_list)
    
    val action = Action.create.roomround_id(gameo.roomround.id.is).actioner_id(currentuserentry.id.is)
                              .mtype(action_enum.toString)
                              .action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                              .action_cards(cards.map(_.card_no.is.toString).mkString(","))
    println("js_action : " + action_enum.toString)
    RoomActor ! SignalAction(gameo.room, action)
    Noop
  }
}

object ActionBraveDeathMatch extends ActionData(MTypeEnum.ACTION_BRAVE_DEATHMATCH, "死鬥") {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is > 0)
  }
  
  override def js_command : JsCmd = js_action
}

object ActionRedKnightBloodPray extends ActionData(MTypeEnum.ACTION_REDKNIGHT_BLOODPRAY, "血腥導言")  {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.heals.is > 0)
  }
  
  override def js_command : JsCmd = js_dialog("redknight_bloodpray_dialog")
}

object ActionRedKnightBloodFeast extends ActionData(MTypeEnum.ACTION_REDKNIGHT_BLOODFEAST, "殺戮盛宴")  {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.yellow_index.is > 0)
  }
  
  override def js_command : JsCmd = js_action
}

object ActionRedKnightDiscipline extends ActionData(MTypeEnum.ACTION_REDKNIGHT_DISCIPLINE, "戒驕戒躁")  {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.tapped.is) && (actioner.gems.is + actioner.crystals.is > 0)
  }
  
  override def js_command : JsCmd = js_action
}

object ActionRedKnightBloodCross extends ActionData(MTypeEnum.ACTION_REDKNIGHT_BLOODCROSS, "腥紅十字") with UserEntryTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.yellow_index.is > 0) && (actioner.gems.is + actioner.crystals.is > 0) &&
    (CardPool.in_hand(actioner, gameo.card_list).filter(_.to_card.cardtype_enum == CardTypeEnum.MAGIC).length >= 2)
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = 
    UserEntry.rr(gameo.userentrys)
  
  override def js_command : JsCmd = js_dialog("redknight_bloodcross_dialog")
}

object ActionSoulMageSoulGive extends ActionData(MTypeEnum.ACTION_SOULMAGE_SOULGIVE, "靈魂賜予") with UserEntryTargetable with CardTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.blue_index.is >= 3)
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = 
    UserEntry.rr(gameo.userentrys)
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.has_action(MTypeEnum.ACTION_SOULMAGE_SOULGIVE))
  }
  
  
  override def js_command : JsCmd = js_dialog("soulmage_soulgive_dialog")
}

object ActionSoulMageSoulBurst extends ActionData(MTypeEnum.ACTION_SOULMAGE_SOULBURST, "靈魂震爆") with UserEntryTargetable with CardTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.yellow_index.is >= 3)
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = 
    UserEntry.rr(gameo.userentrys)
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.has_action(MTypeEnum.ACTION_SOULMAGE_SOULBURST))
  }
  
  override def js_command : JsCmd = js_dialog("soulmage_soulburst_dialog")
}

object ActionSoulMageSoulSummon extends ActionData(MTypeEnum.ACTION_SOULMAGE_SOULSUMMON, "靈魂召還")  {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    //val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    //(hand_cards.filter(x => x.to_card.cardtype_enum == CardTypeEnum.MAGIC).length > 0)
    true
  }
  
  override def js_command : JsCmd = js_dialog("soulmage_soulsummon_dialog")
}

object ActionSoulMageSoulMirror extends ActionData(MTypeEnum.ACTION_SOULMAGE_SOULMIRROR, "靈魂鏡像") with UserEntryTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    (actioner.yellow_index.is >= 2) //&& (hand_cards.length >= 3)
  }
  
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = 
    UserEntry.rr(gameo.userentrys)
  
  override def js_command : JsCmd = js_dialog("soulmage_soulmirror_dialog")
}

object ActionSoulMageSoulLink extends ActionData(MTypeEnum.ACTION_SOULMAGE_SOULLINK, "靈魂鏈結") with UserEntryTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.yellow_index.is > 0) && (actioner.blue_index.is > 0)
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = {
    gameo.userentrys.filter(x=>(!x.revoked.is) && (x.id.is != actioner.id.is) &&
                               (x.team_id.is == actioner.team_id.is))
  }
  
  override def js_command : JsCmd = js_dialog("soulmage_soullink_dialog")
}

object ActionSoulMageSoulLinkTransfer extends ActionData(MTypeEnum.ACTION_SOULMAGE_SOULLINK_TRANSFER, "傷害轉移")  {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.target_user.is != 0 ) && (actioner.blue_index.is != 0)
  }
  
  override def js_command : JsCmd = js_dialog("soulmage_soullink_transfer_dialog")
}

object ActionSoulMageSoulEnhance extends ActionData(MTypeEnum.ACTION_SOULMAGE_SOULENHANCE, "靈魂增幅")  {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is > 0)
  }
  
  override def js_command : JsCmd = js_action
}

object ActionMikoBloodCry extends ActionData(MTypeEnum.ACTION_MIKO_BLOODCRY, "血之悲鳴") with UserEntryTargetable with CardTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.tapped.is)
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = 
    UserEntry.rr(gameo.userentrys)
  
  override def targetable_cards(gameo : GameObject, actioner : UserEntry)  : List[CardPool] = {
    val hand_cards  = CardPool.in_hand(actioner, gameo.card_list)
    hand_cards.filter(x => x.to_card.has_action(MTypeEnum.ACTION_MIKO_BLOODCRY))
  }
  
  override def js_command : JsCmd = js_dialog("miko_bloodcry_dialog")
}

object ActionMikoStickWith extends ActionData(MTypeEnum.ACTION_MIKO_STICKWITH, "同生共死") with UserEntryTargetable {
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = 
    gameo.userentrys.filter(x => (!x.revoked.is) && (x.id.is != actioner.id.is))
  
  override def js_command : JsCmd = js_dialog("miko_stickwith_dialog")
}

object ActionMikoBloodSorrow extends ActionData(MTypeEnum.ACTION_MIKO_BLOODSORROW, "血之哀傷") with UserEntryTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.target_user.is != 0)
  }
  
  override def targetable_users(gameo : GameObject, actioner : UserEntry) : List[UserEntry] = 
   gameo.userentrys.filter(x => (!x.revoked.is) && (x.id.is != actioner.id.is) &&
                                (x.id.is != actioner.target_user.is))
  
  override def js_command : JsCmd = js_dialog("miko_bloodsorrow_dialog")
}

object ActionMikoReverseBleed extends ActionData(MTypeEnum.ACTION_MIKO_REVERSEBLEED, "逆流")  {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.tapped.is)
  }
  
  override def js_command : JsCmd = js_dialog("miko_reversebleed_dialog")
}

object ActionMikoBloodCurse extends ActionData(MTypeEnum.ACTION_MIKO_BLOODCURSE, "血之詛咒") with UserEntryTargetable {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is > 0)
  }
  
  override def js_command : JsCmd = js_dialog("miko_bloodcurse_dialog")
}

object ActionButterflyDance extends ActionData(MTypeEnum.ACTION_BUTTERFLY_DANCE, "舞動") {
  override def js_command : JsCmd = js_dialog("butterfly_dance_dialog")
}

object ActionButterflyPoisonPowder extends ActionData(MTypeEnum.ACTION_BUTTERFLY_POISONPOWDER, "毒粉") {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (CardPool.in_back(actioner, gameo.card_list).length > 0)
  }
  
  override def js_command : JsCmd = js_dialog("butterfly_poisonpowder_dialog")
}

object ActionButterflyPilgrimage extends ActionData(MTypeEnum.ACTION_BUTTERFLY_PILGRIMAGE, "朝聖") {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (CardPool.in_back(actioner, gameo.card_list).length > 0)
  }
  
  override def js_command : JsCmd = js_dialog("butterfly_pilgrimage_dialog")
}

object ActionButterflyMirrorFlower extends ActionData(MTypeEnum.ACTION_BUTTERFLY_MIRRORFLOWER, "鏡花水月") {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (CardPool.in_back(actioner, gameo.card_list).length >= 2)
  }
  
  override def js_command : JsCmd = js_dialog("butterfly_mirrorflower_dialog")
}

object ActionButterflyBackDiscard extends ActionData(MTypeEnum.ACTION_BUTTERFLY_BACKDISCARD, "丟棄蓋牌") {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (CardPool.in_back(actioner, gameo.card_list).length >= RoleButterfly.role_back_cards_max)
  }
  
  override def js_command : JsCmd = js_dialog("butterfly_backdiscard_dialog")
}


object ActionButterflyCocoon extends ActionData(MTypeEnum.ACTION_BUTTERFLY_COCOON, "繭化") {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is > 0)
  }
  
  override def js_command : JsCmd = js_action //js_dialog("butterfly_cocoon_dialog")
}

object ActionButterflyReverseFly extends ActionData(MTypeEnum.ACTION_BUTTERFLY_REVERSEFLY, "倒逆之蝶") {
  override def enabled(gameo : GameObject, actioner : UserEntry)  = {
    (actioner.gems.is + actioner.crystals.is > 0)
  }
  
  override def js_command : JsCmd = js_dialog("butterfly_reversefly_dialog")
}
