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


import scala.xml.{NodeSeq, PrefixedAttribute, Null}

//import collection.mutable.{LinkedList, HashMap, SynchronizedMap}

import org.plummtw.astgrail.enum._
import org.plummtw.astgrail.model._
import org.plummtw.astgrail.util._
import org.plummtw.astgrail.data._
import org.plummtw.astgrail.actor._
import org.plummtw.astgrail.actor._
import org.plummtw.astgrail.card._

import org.plummtw.astgrail.heavy.GameProcessor

class ActionSnippet extends Logger {
  val random = scala.util.Random
  
  /*
  def alert(in: NodeSeq) =
    ajaxButton("Test",
      () => S.runTemplate(List("dialog/test_dialog")).
      map(ns => ModalDialog(ns)) openOr
      Alert("Couldn't find _jsdialog_confirm template"))
  */
  
  def test(in: NodeSeq) =
    bind("confirm", in,
         "yes" -> ((b: NodeSeq) => ajaxButton(b, () =>
          {println("Rhode Island Destroyed")
          Unblock & Alert("Rhode Island Destroyed")})),
         "no" -> ((b: NodeSeq) => <button onclick={Unblock.toJsCmd}>{b}</button>))

  def kick(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get

    var target_str : String = ""
    val targets = ActionKick.targetable_users(gameo, currentuserentry)
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      println("target_str : " + target_str)
      val action = Action.create.roomround_id(gameo.roomround.id.is).mtype(MTypeEnum.ACTION_KICK.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id)
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), targets, x => (target_str = x)),
         "kick" -> ajaxSubmit("踢出", () => { process; Unblock }),
         "cancel" -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  /*
  def attack(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val cards_in_hand = CardPool.in_hand(currentuserentry, gameo.card_list)
    val attack_cards  = ActionAttack.targetable_cards(gameo, currentuserentry)
    val poison_cards  = ActionMagicPoison.targetable_cards(gameo, currentuserentry)
    val weaken_cards  = ActionMagicWeaken.targetable_cards(gameo, currentuserentry)
    val shield_cards  = ActionMagicShield.targetable_cards(gameo, currentuserentry)
    val mbolt_cards   = ActionMagicMBolt.targetable_cards(gameo, currentuserentry)
                                      
    val card_list = cards_in_hand.filter(x =>
      (attack_cards.contains(x)) || (poison_cards.contains(x)) || (weaken_cards.contains(x)) ||
      (shield_cards.contains(x)) || (mbolt_cards.contains(x)))
    val card_map  = card_list.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    var card_str : String = card_map.head._1
      
    val attack_targets = ActionAttack.targetable_users(gameo, currentuserentry)
    val poison_targets = ActionMagicPoison.targetable_users(gameo, currentuserentry)
    val weaken_targets = ActionMagicWeaken.targetable_users(gameo, currentuserentry)
    val shield_targets = ActionMagicShield.targetable_users(gameo, currentuserentry)
    val mbolt_targets  = ActionMagicMBolt.targetable_users(gameo, currentuserentry)
    
    def card_targets(x : CardPool) = {
      if  (attack_cards.contains(x))  attack_targets
      else if (poison_cards.contains(x)) poison_targets
      else if (weaken_cards.contains(x)) weaken_targets
      else if (shield_cards.contains(x)) shield_targets
      else if (mbolt_cards.contains(x)) mbolt_targets
      else List()
    }
    
    val card_targets_map = card_list.map(x => (x.card_no.is.toString, card_targets(x))).toMap
    var target_str : String = ""
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)
      
      val mtype =
        if  (attack_cards.contains(card))     ActionAttack.action_enum
        else if (poison_cards.contains(card)) ActionMagicPoison.action_enum
        else if (weaken_cards.contains(card)) ActionMagicWeaken.action_enum
        else if (shield_cards.contains(card)) ActionMagicShield.action_enum
        else if (mbolt_cards.contains(card))  ActionMagicMBolt.action_enum
        else List()
            
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ATTACK.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is)
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    def replace(state: String): JsCmd = {
      println("In REPLACE : " + state)
      val (name1, js1) = ajaxCall(JE.JsRaw("this.value"),
                            s => After(200, replace(s)))
      SetHtml("card_select", SHtml.select(card_map,
                             Full(card_str),  x => card_str = x, "onchange" -> js1.toJsCmd)) &
      SetHtml("user_select_table", UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
              card_targets_map.get(state).get, x => (target_str = x)))
    }
    
    val (name, js) = ajaxCall(JE.JsRaw("this.value"),
                              s => After(200, replace(s)))
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x, "onchange" -> js.toJsCmd),
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                card_targets_map.get(card_str).get, x => (target_str = x)),
         "attack"    -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  } */
 
  def attack(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    var flag2_str = ""
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_cards  = ActionAttack.targetable_cards(gameo, currentuserentry)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    var card_str : String = card_map.head._1
      
    val attack_targets = ActionAttack.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    val userentrys_rr = UserEntry.rr(gameo.userentrys)
    
    def process : JsCmd = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      if (target_id == 0)
        return Unblock & Alert("無目標，請重試")
      
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)
      
      if ((currentuserentry.get_role == RoleArcher) &&
          ((flag2_str == MTypeEnum.ACTION_ARCHER_AIM.toString) &&
           (!card.to_card.has_action(MTypeEnum.ACTION_ARCHER_AIM)))) {
        warn("NO AIM")
        warn(currentuserentry.get_role.toString)
        warn(flag2_str)
        warn(card.to_card.card_enum.toString)
        return Unblock & Alert("卡片未對應精準射擊")
      }
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ATTACK.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is)
                               .action_flags2(flag2_str)
                               
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    val current_role = currentuserentry.get_role
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, 
                                attack_targets, x => (target_str = x)),
         "specific"  -> (if (current_role == RoleArcher)
                           <span>精準射擊{SHtml.checkbox(false, (if (_) flag2_str = MTypeEnum.ACTION_ARCHER_AIM.toString))}</span>
                         else if (current_role == RoleSaintGirl)
                           <span>冰霜禱言{SHtml.select(userentrys_rr.map(userentry1 => (userentry1.id.is.toString, userentry1.handle_name.is)), 
                                          Full(userentrys_rr.head.id.is.toString), x => flag2_str = x)}</span>
                         else if ((current_role == RoleSaintLance) && (currentuserentry.hasnt_role_flag(UserEntryRoleFlagEnum.SAINTLANCE_NOSKYLANCE))
                                                                                    && (currentuserentry.heals.is >= 2))
                           <span>天槍{SHtml.checkbox(false, (if (_) flag2_str = MTypeEnum.ACTION_SAINTLANCE_SKYLANCE.toString))}</span>
                         else if ((current_role == RoleMagicSword) && (currentuserentry.gems.is > 0) &&
                                                                                 (currentuserentry.hasnt_role_flag(UserEntryRoleFlagEnum.MAGICSWORD_DARKSTUNNED)))
                           <span>黑暗震顫{SHtml.checkbox(false, (if (_) flag2_str = MTypeEnum.ACTION_MAGICSWORD_DARKSTUN.toString))}</span>
                         else if ((current_role == RoleSwordEmp) && (currentuserentry.gems.is + currentuserentry.crystals.is > 0) && 
                                  (CardPool.in_back(currentuserentry, gameo.card_list).length >0 )) {
                           if ((currentuserentry.gems.is + currentuserentry.crystals.is) % 2 == 1)
                           <span>天使之魂{SHtml.checkbox(false, (if (_) flag2_str = MTypeEnum.ACTION_SWORDEMP_ANGELSOUL.toString))}</span>
                           else
                           <span>惡魔之魂{SHtml.checkbox(false, (if (_) flag2_str = MTypeEnum.ACTION_SWORDEMP_DEVILSOUL.toString))}</span>
                         } else if (current_role == RoleMonk) {
                           var option_seq = Seq(("", "不使用"))
                           if ((currentuserentry.yellow_index.is < RoleMonk.role_yellow_index_max) && (!currentuserentry.tapped.is))
                             option_seq = option_seq ++ Seq((MTypeEnum.ACTION_MONK_POWERUP.toString, "蓄力一擊"))
                           if (currentuserentry.yellow_index.is > 0)
                             option_seq = option_seq ++ Seq((MTypeEnum.ACTION_MONK_FIRESOUL.toString ,"蒼炎之魂"))
                           SHtml.select(option_seq, Full(""), x => flag2_str = x)
                         } else if (current_role == RoleBrave) {
                           <span>{
                           if (currentuserentry.yellow_index.is > 0)
                              <span>怒吼{SHtml.checkbox(false, (if (_) flag2_str += MTypeEnum.ACTION_BRAVE_ROAR.toString + ","))}</span>
                           else <span></span>}{
                           if (currentuserentry.blue_index.is >= 4)
                              <span>明鏡止水{SHtml.checkbox(false, (if (_) flag2_str += MTypeEnum.ACTION_BRAVE_STILLWATER.toString + ","))}</span>
                           else <span></span>}
                           </span>
                         } else if (current_role == RoleSoulMage) {
                           var option_seq = Seq(("", "不轉換"))
                           if (currentuserentry.yellow_index.is > 0)
                             option_seq = option_seq ++ Seq((MTypeEnum.ACTION_SOULMAGE_SOULCONVERTY.toString, "黃轉藍"))
                           if (currentuserentry.blue_index.is > 0)
                             option_seq = option_seq ++ Seq((MTypeEnum.ACTION_SOULMAGE_SOULCONVERTB.toString ,"藍轉黃"))
                           SHtml.select(option_seq, Full(""), x => flag2_str = x)
                         } else <span></span>),
         "attack"    -> ajaxSubmit("確定", () => process),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def reattack(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    var flag2_str = ""
    
    val card_map = ActionReattack.targetable_cards(gameo, currentuserentry)
                                .map(x => (x.card_no.is.toString, CardEnum.get_card(x.card.is).card_name))
      
    var card_str : String = card_map.head._1
      
    var target_str : String = ""
    //val currentuserentry = CurrentUserEntry_R.get
    val targets = ActionReattack.targetable_users(gameo, currentuserentry)

    val userentrys_rr = UserEntry.rr(gameo.userentrys)
    
    def process : JsCmd = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      if (target_id == 0)
        return Unblock & Alert("無目標，請重試")
      
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)
      
      if ((currentuserentry.get_role == RoleArcher) &&
          ((flag2_str == MTypeEnum.ACTION_ARCHER_AIM.toString) &&
           (!card.to_card.has_action(MTypeEnum.ACTION_ARCHER_AIM)))) {
        warn("NO AIM")
        warn(currentuserentry.get_role.toString)
        warn(flag2_str)
        warn(card.to_card.card_enum.toString)
        return Unblock & Alert("卡片未對應精準射擊")
      }
            
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_REATTACK.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is)
                               .action_flags2(flag2_str)
      
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), targets, x => (target_str = x)),
         "specific"  -> (if (currentuserentry.get_role == RoleArcher)
                           <span>精準射擊{SHtml.checkbox(false, (if (_) flag2_str = MTypeEnum.ACTION_ARCHER_AIM.toString))}</span>
                         else if (currentuserentry.get_role == RoleSaintGirl)
                           <span>冰霜禱言{SHtml.select(userentrys_rr.map(userentry1 => (userentry1.id.is.toString, userentry1.handle_name.is)), 
                                          Full(userentrys_rr.head.id.is.toString), x => flag2_str = x)}</span>
                         else <span></span>),
         "reattack"  -> ajaxSubmit("攻擊(應戰)", () => process ),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def light(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    val card_map = ActionLight.targetable_cards(gameo, currentuserentry)
                              .map(x => (x.card_no.is.toString, CardEnum.get_card(x.card.is).card_name))
      
    var card_str : String = card_map.head._1
      
    def process = {
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)
            
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_LIGHT.toString)
                               .actioner_id(currentuserentry.id.is)
                               .action_card(card.id.is).action_flags(card.card.is)
                               .action_flags2(gameo.roomphase.last_phase_type.is)
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "light"     -> ajaxSubmit("聖光", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def magic_poison(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_cards  = ActionMagicPoison.targetable_cards(gameo, currentuserentry)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    var card_str : String = card_map.head._1
      
    val attack_targets = ActionMagicPoison.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_MAGIC_POISON.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is)
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "poison"    -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def magic_weaken(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_cards  = ActionMagicWeaken.targetable_cards(gameo, currentuserentry)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    var card_str : String = card_map.head._1
      
    val attack_targets = ActionMagicWeaken.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_MAGIC_WEAKEN.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is)
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "weaken"    -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def magic_mbolt(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_cards  = ActionMagicMBolt.targetable_cards(gameo, currentuserentry)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    var card_str : String = card_map.head._1
      
    val attack_targets = ActionMagicMBolt.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    def process : JsCmd = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      if (target_id == 0)
        return Unblock & Alert("無目標，請重試")
      
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_MAGIC_MBOLT.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is)
      if ((attack_targets.length == 2) &&
          (target_id == attack_targets(1).id.is))
        action.action_flags2(RoomRoundFlagEnum.MBOLT_REVERSE.toString)
      
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "mbolt"    -> ajaxSubmit("確定", () => { process }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def magic_shield(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_cards  = ActionMagicShield.targetable_cards(gameo, currentuserentry)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    var card_str : String = card_map.head._1
      
    val attack_targets = ActionMagicShield.targetable_users(gameo, currentuserentry)
    var target_str : String = ""

    val userentrys_rr = UserEntry.rr(gameo.userentrys)    
    var flag2_str  : String = ""
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_MAGIC_SHIELD.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is)
      if (currentuserentry.get_role == RoleAngel)
        action.action_flags2(flag2_str)
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "specific"  ->  (if (currentuserentry.get_role == RoleAngel)
                           <span>天使羈絆{SHtml.select(userentrys_rr.map(userentry1 => (userentry1.id.is.toString, userentry1.handle_name.is)), 
                                          Full(userentrys_rr.head.id.is.toString), x => flag2_str = x)}</span> 
                          else <span></span>),
         "shield"    -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def combine(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentryteam = UserEntryTeam.get(currentuserentry.team_id, gameo.userentryteams)
    
    var combine_list : List[GameEnum.Value] = List()
    if (userentryteam.crystals.is >= 3)
      combine_list = combine_list ::: List(GameEnum.CRYSTAL3)
    if ((userentryteam.gems.is >= 1) && (userentryteam.crystals.is >= 2))
      combine_list = combine_list ::: List(GameEnum.GEM1CRYSTAL2)
    if ((userentryteam.gems.is >= 2) && (userentryteam.crystals.is >= 1))
      combine_list = combine_list ::: List(GameEnum.GEM2CRYSTAL1)
    if (userentryteam.gems.is >= 3)
      combine_list = combine_list ::: List(GameEnum.GEM3)
    
    var combine_map = combine_list.map(x => (x.toString, GameEnum.combine_cname((x))))
      
    var combine_str : String = combine_map.head._1
      
    def process = {
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_COMBINE.toString)
                               .actioner_id(currentuserentry.id.is).action_flags(combine_str)
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    
    ajaxForm(bind("action", in,
         "combine_select" -> SHtml.select(combine_map,
                             Full(combine_str),  x => combine_str = x),
         "combine"   -> ajaxSubmit("合成", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def refine(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val role = currentuserentry.get_role
    val current_energy = currentuserentry.gems.is + currentuserentry.crystals.is
    val userentryteam = UserEntryTeam.get(currentuserentry.team_id, gameo.userentryteams)
    
    val teamuserentrys = gameo.userentrys.filter(_.team_id.is == currentuserentry.team_id.is)
    var target_str : String = teamuserentrys.head.id.is.toString 
    
    val can_crystal1 = 
      if ((role.role_is_gem_only) && (current_energy >= 2) && (currentuserentry.gems.is == 0))
        false
      else true
    val can_crystal2 = 
      if ((role.role_is_gem_only) && (current_energy >= 1) && (currentuserentry.gems.is == 0))
        false
      else true
      
    var refine_list : List[GameEnum.Value] = List()
    //if (role == RoleAdventurer) {
      if ((userentryteam.crystals.is >= 1))
        refine_list ++= List(GameEnum.CRYSTAL1)
      if (userentryteam.gems.is >= 1)
        refine_list ++= List(GameEnum.GEM1)
      if ((userentryteam.crystals.is >= 2))
        refine_list ++= List(GameEnum.CRYSTAL2)
      if ((userentryteam.gems.is >= 1) && (userentryteam.crystals.is >= 1))
        refine_list ++= List(GameEnum.GEM1CRYSTAL1)
      if ((userentryteam.gems.is >= 2))
        refine_list ++= List(GameEnum.GEM2)
    /* }  else {
      if ((userentryteam.crystals.is >= 1) && can_crystal1) //(!role.role_is_gem_only))
        refine_list ++= List(GameEnum.CRYSTAL1)
      if (userentryteam.gems.is >= 1)
        refine_list ++= List(GameEnum.GEM1)
      if ((userentryteam.crystals.is >= 2) && (current_energy < 2) && can_crystal2) //&& (!role.role_is_gem_only))
        refine_list ++= List(GameEnum.CRYSTAL2)
      if ((userentryteam.crystals.is >= 1) && (userentryteam.gems.is >= 1) && (current_energy < 2)) // && (!role.role_is_gem_only))
        refine_list ++= List(GameEnum.GEM1CRYSTAL1)
      if ((userentryteam.gems.is >= 2) && (current_energy < 2))
        refine_list ++= List(GameEnum.GEM2)
    } */
    
    var refine_map = refine_list.map(x => (x.toString, GameEnum.combine_cname((x))))
      
    var refine_str : String = refine_map.head._1
      
    def process : JsCmd = {
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_REFINE.toString)
                               .actioner_id(currentuserentry.id.is).action_flags(refine_str)
                               
      if (role == RoleAdventurer) {
        val target_id : Long = try {
          target_str.toLong 
        } catch {case e: Exception => 0}
        
        val actionee = UserEntry.get(target_id, gameo.userentrys)
        if (actionee.gems.is + actionee.crystals.is + refine_str.length > actionee.energy_max)
          return Unblock & Alert("超過目標能量上限")
        
        action.actionee_id(target_id)
      } else {
        if (currentuserentry.gems.is + currentuserentry.crystals.is + refine_str.length > currentuserentry.energy_max)
          return Unblock & Alert("超過能量上限")
      }
      
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    
    ajaxForm(bind("action", in,
         "refine_select"       -> SHtml.select(refine_map,
                                Full(refine_str),  x => refine_str = x),
         "user_select"         -> 
           (if (currentuserentry.get_role == RoleAdventurer)
             <span>交給{SHtml.select(teamuserentrys.map(userentry1 => (userentry1.id.is.toString, userentry1.handle_name.is)), 
                                          Full(target_str), x => target_str = x)}</span>
             else <span></span>),
         "refine"    -> ajaxSubmit("提煉", () => process),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def discard(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val cards_in_hand = CardPool.in_hand(currentuserentry, gameo.card_list)
    val discard_number = math.max(0, cards_in_hand.length - currentuserentry.get_hand_max)
    
    var card_choose_list : List[Int] = List()
    
    def card_choose = {
      <div align="center"><table> {
       for (card_group <- cards_in_hand.grouped(4)) yield
         <tr>{ for (card <- card_group) yield
           <td>{SHtml.checkbox(false, x => if (x) card_choose_list = card_choose_list ::: List(card.card_no.is))}{
               card.to_card.card_name.toString}</td> }</tr>
      } </table></div>
    }
      
    def process = {
      val cards = card_choose_list.map(CardPool.getByNo(_, gameo.card_list))
                
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_DISCARD.toString)
                               .actioner_id(currentuserentry.id.is)
                               .action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                               .action_cards(card_choose_list.map(_.toString).mkString(","))
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    
    ajaxForm(bind("action", in,
         "discard_num" -> <span>{discard_number}</span>,
         "card_select" -> card_choose,
         "discard"   -> ajaxSubmit("棄牌", () => { 
                          if (card_choose_list.length != discard_number) 
                            Unblock & Alert("你選擇了" + card_choose_list.length + "張，選擇數不為 " + discard_number.toString)
                          else {
                            process ; Unblock
                          }
                        }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def heal(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)

    var heal_max =
      math.min(currentuserentry.heals.is, roomphase.power.is)
    
    if (currentuserentry.get_role == RoleBishop)
      heal_max = 1
    
    var heal_map = Range(0, heal_max+1).reverse.map(x => (x.toString, x.toString))
      
    var heal_str : String = heal_map.head._1
      
    def process = {
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_HEAL.toString)
                               .actioner_id(currentuserentry.id.is).action_flags(heal_str)
                               .action_flags2(gameo.roomphase.last_phase_type.is)
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    
    ajaxForm(bind("action", in,
         "power"             -> <span>{gameo.roomphase.power.is}</span>,
         "heal_select"       -> SHtml.select(heal_map,
                                Full(heal_str),  x => heal_str = x),
         "heal"      -> ajaxSubmit("治療", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def trap(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_cards  = ActionArcherTrap.targetable_cards(gameo, currentuserentry)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    var card_str : String = card_map.head._1
      
    val attack_targets = ActionArcherTrap.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ARCHER_TRAP.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is)
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "trap"    -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def snipe(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_targets = ActionArcherSnipe.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ARCHER_SNIPE.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               //.action_card(card.id.is).action_flags(card.card.is)
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "snipe"     -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def penetrate(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_cards  = ActionArcherPenetrate.targetable_cards(gameo, currentuserentry)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    var card_str : String = card_map.head._1
      
    def process = {
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ARCHER_PENETRATE.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(gameo.roomphase.actionee_id.is)
                               .action_card(card.id.is).action_flags(card.card.is)
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "penetrate" -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def watershadow(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val card_list = ActionAssassinWaterShadow.targetable_cards(gameo, currentuserentry)
    //val discard_number = cards_in_hand.length - currentuserentry.hand_max.is
    
    var card_choose_list : List[Int] = List()
    
    def card_choose = {
      <div align="center"><table> {
       for (card_group <- card_list.grouped(4)) yield
         <tr>{ for (card <- card_group) yield
           <td>{SHtml.checkbox(false, x => if (x) card_choose_list = card_choose_list ::: List(card.card_no.is))}{
               card.to_card.card_name.toString}</td> }</tr>
      } </table></div>
    }
      
    def process = {
      val cards = card_choose_list.map(CardPool.getByNo(_, gameo.card_list))
                
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ASSASSIN_WATERSHADOW.toString)
                               .actioner_id(currentuserentry.id.is)
                               .action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                               .action_cards(card_choose_list.map(_.toString).mkString(","))
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    
    ajaxForm(bind("action", in,
         //"discard_num" -> <span>{discard_number}</span>,
         "power"       -> <span>{gameo.roomphase.power.is}</span>,
         "card_select" -> card_choose,
         "watershadow" -> ajaxSubmit("水影", () => { 
                          if (card_choose_list.length == 0) 
                            Unblock & Alert("選擇數不得為 0")
                          else {
                            process ; Unblock
                          }
                        }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def saint_heal(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_cards  = ActionSaintGirlHeal.targetable_cards(gameo, currentuserentry)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    var card_str : String = card_map.head._1
      
    val attack_targets = ActionSaintGirlHeal.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_SAINTGIRL_HEAL.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is)
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "saint_heal" -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def heallight(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_cards  = ActionSaintGirlHealLight.targetable_cards(gameo, currentuserentry)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    var card_str : String = card_map.head._1
      
    val attack_targets = ActionSaintGirlHealLight.targetable_users(gameo, currentuserentry)
    var target_ids : List[Long] = List()
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_SAINTGIRL_HEALLIGHT.toString)
                               .actioner_id(currentuserentry.id.is)
                               .action_card(card.id.is).action_flags(card.card.is)
                               .action_flags2(target_ids.map(_.toString).mkString(","))
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_choose_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_ids = target_ids ::: List(x))),
         "heallight" -> ajaxSubmit("確定", () => {
           if ((target_ids.length >= 1) && (target_ids.length <= 3)) {
             process; Unblock 
           } else
             Unblock & Alert("選擇之玩家數必須為 1 至 3")
           }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def holyheal(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val targets = ActionSaintGirlHolyHeal.targetable_users(gameo, currentuserentry)
    val targets_map = targets.map(userentry1 => (userentry1.id.is.toString, userentry1.handle_name.is))
                
    var target_id1 : String = targets.head.id.is.toString
    var target_id2 : String = targets.head.id.is.toString
    var target_id3 : String = targets.head.id.is.toString
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_SAINTGIRL_HOLYHEAL.toString)
                               .actioner_id(currentuserentry.id.is)
                               //.action_card(card.id.is).action_flags(card.card.is)
                               .action_flags2(target_id1 + "," + target_id2 + "," + target_id3)
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    ajaxForm(bind("action", in,
         "holyheal_select1" -> {SHtml.select(targets_map, 
                                  Full(target_id1), x => target_id1 = x)},
         "holyheal_select2" -> {SHtml.select(targets_map, 
                                  Full(target_id1), x => target_id2 = x)},
         "holyheal_select3" -> {SHtml.select(targets_map, 
                                  Full(target_id1), x => target_id3 = x)},
         "holyheal" -> ajaxSubmit("確定", () => {
             process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }            
  
  def seal(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_cards  = ActionSealerSeal.targetable_cards(gameo, currentuserentry)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    var card_str : String = card_map.head._1
      
    val attack_targets = ActionSealerSeal.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    def process : JsCmd = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)
      val carddata = card.to_card
      
      // 檢查是否已有同屬性的封印
      var same_attr = false
      val front_cards = CardPool.in_front(UserEntry.get(target_id, gameo.userentrys), gameo.card_list)
      front_cards.foreach { front_card =>
        val front_carddata = front_card.to_card
        if (front_carddata.has_action(MTypeEnum.ACTION_SEALER_SEAL) &&
            (front_carddata.cardattr_enum == carddata.cardattr_enum) )
        same_attr = true  
      }
      
      if (same_attr) {
        Unblock & Alert("目標已有同屬性之封印")
      } else {
        val action = 
          Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_SEALER_SEAL.toString)
                                 .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                                 .action_card(card.id.is).action_flags(card.card.is)
        
        RoomActor ! SignalAction(gameo.room, action)
        Unblock
      }
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "seal"      -> ajaxSubmit("確定", () => { process }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def fiveseal(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    val attack_targets = ActionSealerFiveSeal.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    def process : JsCmd = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_SEALER_FIVESEAL.toString)
                                  .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                                   //.action_card(card.id.is).action_flags(card.card.is)
        
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         //"card_select"       -> SHtml.select(card_map,
         //                       Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "fiveseal"  -> ajaxSubmit("確定", () => { process }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def breakseal(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys = gameo.userentrys
    
    //println("DEBUG attack : " + currentuserentry.toString)
    val attack_cards  = ActionSealerBreakSeal.targetable_cards(gameo, currentuserentry)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, 
                                           UserEntry.get(x.target_id, userentrys).handle_name.is + " 的 " + x.to_card.card_name))
      
    var card_str : String = card_map.head._1
      
    //val attack_targets = ActionSealerSeal.targetable_users(gameo, currentuserentry)
    //var target_str : String = ""
    
    def process : JsCmd = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      //val target_id : Long = try {
      //  target_str.toLong 
      //} catch {case e: Exception => 0}
      
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)
      val carddata = card.to_card
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_SEALER_BREAKSEAL.toString)
                                 .actioner_id(currentuserentry.id.is).actionee_id(card.target_id.is)
                                 .action_card2(card.id.is).action_flags(card.card.is)
        
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         //"user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
         //                       attack_targets, x => (target_str = x)),
         "breakseal" -> ajaxSubmit("確定", () => { process }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def shock(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_cards  = ActionMageShock.targetable_cards(gameo, currentuserentry)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    var card_str : String = card_map.head._1
      
    val attack_targets = ActionMageShock.targetable_users(gameo, currentuserentry)
    var target_ids : List[Long] = List() //String = ""
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      //val target_id : Long = try {
      //  target_str.toLong 
      //} catch {case e: Exception => 0}
      
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_MAGE_SHOCK.toString)
                               .actioner_id(currentuserentry.id.is) //.actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is)
                               .action_flags2(target_ids.map(_.toString).mkString(","))
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_choose_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_ids = target_ids ::: List(x))),
         "shock"    -> ajaxSubmit("確定",  () => {
           if (target_ids.length == 2) {
             process; Unblock 
           } else
             Unblock & Alert("選擇之玩家數必須為 2")
           }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def shock_discard(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_cards  = ActionMageShockDiscard.targetable_cards(gameo, currentuserentry)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    var card_str : String = card_map.head._1
      
    //val attack_targets = ActionMageShock.targetable_users(gameo, currentuserentry)
    //var target_ids : List[Long] = List() //String = ""
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      //val target_id : Long = try {
      //  target_str.toLong 
      //} catch {case e: Exception => 0}
      
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_MAGE_SHOCK_DISCARD.toString)
                               .actioner_id(currentuserentry.id.is) //.actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is)
                               //.action_flags2(target_ids.map(_.toString).mkString(","))
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         //"user_select_table" -> UserEntryHelper.user_choose_table(UserEntry.rr(gameo.userentrys), 
         //                       attack_targets, x => (target_ids = target_ids ::: List(x))),
         "shock_discard"-> ajaxSubmit("確定",  () => {process;  Unblock}),
         "cancel"       -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def storm(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_targets = ActionMageStorm.targetable_users(gameo, currentuserentry)
    var target_ids : List[Long] = List() //String = ""
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      //val target_id : Long = try {
      //  target_str.toLong 
      //} catch {case e: Exception => 0}
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_MAGE_STORM.toString)
                               .actioner_id(currentuserentry.id.is) //.actionee_id(target_id)
                               //.action_card(card.id.is).action_flags(card.card.is)
                               .action_flags2(target_ids.map(_.toString).mkString(","))
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    ajaxForm(bind("action", in,
         //"card_select"       -> SHtml.select(card_map,
         //                       Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_choose_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_ids = target_ids ::: List(x))),
         "storm"    -> ajaxSubmit("確定",  () => {
           if (target_ids.length == 2) {
             process; Unblock 
           } else
             Unblock & Alert("選擇之玩家數必須為 2")
           }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def windpurify(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    val card_map = ActionAngelWindPurify.targetable_cards(gameo, currentuserentry)
                                        .map(x => (x.card_no.is.toString, CardEnum.get_card(x.card.is).card_name))
    var card_str : String = card_map.head._1
    
    val card_map2  = ActionAngelWindPurify.targetable_cards2(gameo, currentuserentry)
                                          .map(x => (x.card_no.is.toString, 
                                             UserEntry.get(x.target_id, gameo.userentrys).handle_name.is + " 的 " + x.to_card.card_name))
    var card_str2 : String = card_map2.head._1

                
    var target_str : String = ""
    val targets = UserEntry.rr(gameo.userentrys)
    
    def process = {
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)

      val card_no2 : Int = try {
        card_str2.toInt
      } catch {case e: Exception => 0}
      
      val card2 = CardPool.getByNo(card_no2, gameo.card_list)
                  
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ANGEL_WINDPURIFY.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is)
                               .action_card2(card2.id.is).action_flags2(card2.card.is)
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), targets, x => (target_str = x)),
         "card_select2"  ->  <span>移除效果{SHtml.select(card_map2, 
                                          Full(card_str2),  x => card_str2 = x)}</span>, 
         "windpurify"  -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"      -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def angelbless(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    val card_map = ActionAngelAngelBless.targetable_cards(gameo, currentuserentry)
                                        .map(x => (x.card_no.is.toString, CardEnum.get_card(x.card.is).card_name))
    var card_str : String = card_map.head._1
    
    val attack_targets = ActionAngelAngelBless.targetable_users(gameo, currentuserentry)
    var target_ids : List[Long] = List() //String = ""
    
    def process = {
      //val target_id : Long = try {
      //  target_str.toLong 
      //} catch {case e: Exception => 0}
      
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)

      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ANGEL_ANGELBLESS.toString)
                               .actioner_id(currentuserentry.id.is) // .actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is)
                               .action_flags2(target_ids.map(_.toString).mkString(","))
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_choose_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_ids = target_ids ::: List(x))),
         "angelbless"  -> ajaxSubmit("確定", () => {
           if ((target_ids.length >=1) && (target_ids.length <=2)) {
             process; Unblock 
           } else
             Unblock & Alert("選擇之玩家數必須為 1 或 2")
           }),
         "cancel"      -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def angelbless_give(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    val cards_in_hand = CardPool.in_hand(currentuserentry, gameo.card_list)
    val give_number = math.min(cards_in_hand.length, gameo.roomphase.power.is)
    
    //val attack_cards  = ActionAngelAngelBlessGive.targetable_cards(gameo, currentuserentry)
    //val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    //var card_str : String = card_map.head._1
    var card_choose_list : List[Int] = List()
    
    def card_choose = {
      <div align="center"><table> {
       for (card_group <- cards_in_hand.grouped(4)) yield
         <tr>{ for (card <- card_group) yield
           <td>{SHtml.checkbox(false, x => if (x) card_choose_list = card_choose_list ::: List(card.card_no.is))}{
               card.to_card.card_name.toString}</td> }</tr>
      } </table></div>
    }
                
      
    //val attack_targets = ActionMageShock.targetable_users(gameo, currentuserentry)
    //var target_ids : List[Long] = List() //String = ""
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      //val target_id : Long = try {
      //  target_str.toLong 
      //} catch {case e: Exception => 0}
      
      //val card_no : Int = try {
      //  card_str.toInt
      //} catch {case e: Exception => 0}
      
      //val card = CardPool.getByNo(card_no, gameo.card_list)
      val cards = card_choose_list.map(CardPool.getByNo(_, gameo.card_list))      
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ANGEL_ANGELBLESS_GIVE.toString)
                               .actioner_id(currentuserentry.id.is)
                               .actionee_id(gameo.roomphase.actionee_id.is)
                               .action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                               .action_cards(card_choose_list.map(_.toString).mkString(","))
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    ajaxForm(bind("action", in,
         "give_num"    -> <span>{give_number}</span>,
         "card_select" -> card_choose,
         "angelbless_give" -> ajaxSubmit("給予", () => { 
                          if (card_choose_list.length != give_number) 
                            Unblock & Alert("選擇數不為 " + give_number.toString)
                          else {
                            process ; Unblock
                          }
                        }),
         "cancel"      -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def angelsong(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    val card_map  = ActionAngelAngelSong.targetable_cards(gameo, currentuserentry)
                                          .map(x => (x.card_no.is.toString, 
                                             UserEntry.get(x.target_id, gameo.userentrys).handle_name.is + " 的 " + x.to_card.card_name))
    var card_str : String = card_map.head._1
    
    var target_str : String = ""
    val targets = UserEntry.rr(gameo.userentrys)
    
    def process = {
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)

      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ANGEL_ANGELSONG.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               .action_flags(card.card.is)
                               .action_card2(card.id.is) // Note : 這邊使用 card2 以免受封印師影響
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), targets, x => (target_str = x)),
         "card_select"  -> <span>移除效果{SHtml.select(card_map, 
                                          Full(card_str),  x => card_str = x)}</span> ,
         "angelsong"  -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"      -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def godcover(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)

    var cover_max =
      math.min((currentuserentry.gems.is + currentuserentry.crystals.is), roomphase.power.is)
    
    var cover_map = Range(0, cover_max+1).reverse.map(x => (x.toString, x.toString))
      
    var cover_str : String = cover_map.head._1
      
    def process = {
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ANGEL_GODCOVER.toString)
                               .actioner_id(currentuserentry.id.is).action_flags(cover_str)
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    
    ajaxForm(bind("action", in,
         "godcover_select"       -> SHtml.select(cover_map,
                                Full(cover_str),  x => cover_str = x),
         "godcover"  -> ajaxSubmit("神之庇護", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def shine(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_cards  = ActionSaintLanceShine.targetable_cards(gameo, currentuserentry)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    var card_str : String = card_map.head._1
      
    //val attack_targets = ActionSaintLanceRetribution.targetable_users(gameo, currentuserentry)
    //var target_str : String = ""
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      //val target_id : Long = try {
      //  target_str.toLong 
      //} catch {case e: Exception => 0}
      
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_SAINTLANCE_SHINE.toString)
                               .actioner_id(currentuserentry.id.is) // .actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is)
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         //"user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
         //                       attack_targets, x => (target_str = x)),
         "shine"    -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def earthlance(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)

    var earthlance_max =
      math.min(currentuserentry.heals.is, 4)
    
    var earthlance_map = Range(1, earthlance_max+1).reverse.map(x => (x.toString, x.toString))
      
    var earthlance_str : String = earthlance_map.head._1
      
    def process = {
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_SAINTLANCE_EARTHLANCE.toString)
                               .actioner_id(currentuserentry.id.is).action_flags(earthlance_str)
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    
    ajaxForm(bind("action", in,
         "earthlance_select" -> SHtml.select(earthlance_map,
                                Full(earthlance_str),  x => earthlance_str = x),
         "earthlance"        -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def retribution(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_cards  = ActionSaintLanceRetribution.targetable_cards(gameo, currentuserentry)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    var card_str : String = card_map.head._1
      
    val attack_targets = ActionSaintLanceRetribution.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_SAINTLANCE_RETRIBUTION.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is)
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "retribution"    -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def elem_magic(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntry.rr(gameo.userentrys)
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_cards  = ActionElementalistMagic.targetable_cards(gameo, currentuserentry)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    var card_str : String = card_map.head._1
      
    val attack_targets = ActionElementalistMagic.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    val card_map2  = Seq(("", "不棄牌")) ++ CardPool.in_hand(currentuserentry,gameo.card_list)
                                                  .map(x => (x.card_no.is.toString, x.to_card.card_name))
    var card_str2 : String = ""
    var flag2_str : String = ""
    
    def process : JsCmd = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)
      
      var card2 : CardPool = null
                              
      if (card_str2 != "") {
        val card_no2 = PlummUtil.parseInt(card_str2)
        card2    = CardPool.getByNo(card_no2, gameo.card_list)
        
        if (card_no == card_no2)
          return Unblock & Alert("棄牌不可與出牌相同")
                                
        if  (card2.to_card.cardattr_enum != card.to_card.cardattr_enum) 
          return Unblock & Alert("棄牌須與出牌屬性相同")
      }
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ELEMENTALIST_MAGIC.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is)
                               .action_flags2(flag2_str)
      if (card2 != null)
        action.action_cards(card2.card_no.is.toString).action_flags(card.card.is + "," + card2.card.is)
      
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "card_select2" -> SHtml.select(card_map2,
                                Full(card_str2),  x => card_str2 = x),
         "user_select2" -> <span>冰凍治癒{SHtml.select(userentrys_rr.map(userentry1 => (userentry1.id.is.toString, userentry1.handle_name.is)), 
                                          Full(userentrys_rr.head.id.is.toString), x => flag2_str = x)}</span>,
         "elem_magic" -> ajaxSubmit("確定", () => process),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def ignite(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
      
    val attack_targets = ActionElementalistIgnite.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      //val card_no : Int = try {
      //  card_str.toInt
      //} catch {case e: Exception => 0}
      
      //val card = CardPool.getByNo(card_no, gameo.card_list)
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ELEMENTALIST_IGNITE.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               //.action_card(card.id.is).action_flags(card.card.is)
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "ignite"    -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def moonlight(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_targets = ActionElementalistMoonLight.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      //val card_no : Int = try {
      //  card_str.toInt
      //} catch {case e: Exception => 0}
      
      //val card = CardPool.getByNo(card_no, gameo.card_list)
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ELEMENTALIST_MOONLIGHT.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               //.action_card(card.id.is).action_flags(card.card.is)
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "moonlight"    -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"       -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def plague(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_cards  = ActionNecromancerPlague.targetable_cards(gameo, currentuserentry)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    var card_str : String = card_map.head._1
      
    //val attack_targets = ActionSaintLanceRetribution.targetable_users(gameo, currentuserentry)
    //var target_str : String = ""
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      //val target_id : Long = try {
      //  target_str.toLong 
      //} catch {case e: Exception => 0}
      
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_NECROMANCER_PLAGUE.toString)
                               .actioner_id(currentuserentry.id.is) //.actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is)
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         //"user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
         //                       attack_targets, x => (target_str = x)),
         "plague"    -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def deathtouch(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val cards_in_hand = CardPool.in_hand(currentuserentry, gameo.card_list)
    //val discard_number = cards_in_hand.length - currentuserentry.get_hand_max
    
    var card_choose_list : List[Int] = List()
    
    def card_choose = {
      <div align="center"><table> {
       for (card_group <- cards_in_hand.grouped(4)) yield
         <tr>{ for (card <- card_group) yield
           <td>{SHtml.checkbox(false, x => if (x) card_choose_list = card_choose_list ::: List(card.card_no.is))}{
               card.to_card.card_name.toString}</td> }</tr>
      } </table></div>
    }
    
    var heal_max = currentuserentry.heals.is
    
    var heal_map = Range(2, heal_max+1).reverse.map(x => (x.toString, x.toString))
      
    var heal_str : String = heal_map.head._1
      
    val attack_targets = ActionNecromancerDeathTouch.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    def process : JsCmd = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val cards = card_choose_list.map(CardPool.getByNo(_, gameo.card_list))
      
      if (card_choose_list.length < 2) {
        return Unblock & Alert("選擇卡片至少須 2 張")
      }
      
      val card_attr = cards(0).to_card.cardattr_enum
      cards.foreach { card1 =>
        val card_attr1 = card1.to_card.cardattr_enum
        if (card_attr1 != card_attr)
          return Unblock & Alert("選擇卡片須同屬性")
      }
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_NECROMANCER_DEATHTOUCH.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               //.action_card(card.id.is).action_flags(card.card.is)
                               .action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                               .action_cards(card_choose_list.map(_.toString).mkString(","))
                               .action_flags2(heal_str)
      
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select"  -> card_choose,
         "user_select" -> SHtml.select(attack_targets.map(userentry1 => (userentry1.id.is.toString, userentry1.handle_name.is)), 
                                       Full(attack_targets.head.id.is.toString), x => target_str = x),
         "heal_select" -> SHtml.select(heal_map,
                                Full(heal_str),  x => heal_str = x),
         "deathtouch" -> ajaxSubmit("確定", () => process),
         "cancel"     -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def comet(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val cards_in_hand = ActionMagicSwordComet.targetable_cards(gameo, currentuserentry)
    //val discard_number = cards_in_hand.length - currentuserentry.get_hand_max
    
    var card_choose_list : List[Int] = List()
    
    def card_choose = {
      <div align="center"><table> {
       for (card_group <- cards_in_hand.grouped(4)) yield
         <tr>{ for (card <- card_group) yield
           <td>{SHtml.checkbox(false, x => if (x) card_choose_list = card_choose_list ::: List(card.card_no.is))}{
               card.to_card.card_name.toString}</td> }</tr>
      } </table></div>
    }
    
    //var heal_max = currentuserentry.heals.is
    
    //var heal_map = Range(2, heal_max+1).reverse.map(x => (x.toString, x.toString))
      
    //var heal_str : String = heal_map.head._1
      
    val attack_targets = ActionMagicSwordComet.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    def process : JsCmd = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val cards = card_choose_list.map(CardPool.getByNo(_, gameo.card_list))
      
      if (card_choose_list.length != 2) {
        return Unblock & Alert("選擇卡片須 2 張")
      }
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_MAGICSWORD_COMET.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               //.action_card(card.id.is).action_flags(card.card.is)
                               .action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                               .action_cards(card_choose_list.map(_.toString).mkString(","))
                               //.action_flags2(heal_str)
      
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select"  -> card_choose,
         "user_select" -> SHtml.select(attack_targets.map(userentry1 => (userentry1.id.is.toString, userentry1.handle_name.is)), 
                                       Full(attack_targets.head.id.is.toString), x => target_str = x),
         "comet"      -> ajaxSubmit("確定", () => process),
         "cancel"     -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def finaljudge(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    //val attack_cards  = ActionMagicPoison.targetable_cards(gameo, currentuserentry)
    //val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    //var card_str : String = card_map.head._1
      
    val attack_targets = ActionJudicatorFinalJudge.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      //val card_no : Int = try {
      //  card_str.toInt
      //} catch {case e: Exception => 0}
      
      //val card = CardPool.getByNo(card_no, gameo.card_list)
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_JUDICATOR_FINALJUDGE.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               //.action_card(card.id.is).action_flags(card.card.is)
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "finaljudge" -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"     -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def balance(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)

    //var heal_max =
    //  math.min(currentuserentry.heals.is, roomphase.power.is)
    
    var balance_map = Seq(("1", "棄掉你的所有手牌"), ("2", "將你的手牌補到上限"))
      
    var balance_str : String = balance_map.head._1
      
    def process = {
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_JUDICATOR_BALANCE.toString)
                               .actioner_id(currentuserentry.id.is).action_flags(balance_str)
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    
    ajaxForm(bind("action", in,
         "balance_select"       -> SHtml.select(balance_map,
                                Full(balance_str),  x => balance_str = x),
         "balance"   -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def deceive(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val cards_in_hand = CardPool.in_hand(currentuserentry, gameo.card_list)
    //val discard_number = cards_in_hand.length - currentuserentry.get_hand_max
    
    var card_choose_list : List[Int] = List()
    
    def card_choose = {
      <div align="center"><table> {
       for (card_group <- cards_in_hand.grouped(4)) yield
         <tr>{ for (card <- card_group) yield
           <td>{SHtml.checkbox(false, x => if (x) card_choose_list = card_choose_list ::: List(card.card_no.is))}{
               card.to_card.card_name.toString}</td> }</tr>
      } </table></div>
    }
    
    var attr_map = List(CardAttrEnum.FIRE, CardAttrEnum.WATER,
                        CardAttrEnum.AIR,  CardAttrEnum.THUNDER,
                        CardAttrEnum.EARTH).map(x => (x.toString, CardAttrEnum.cname(x.toString)))
      
    var attr_str : String = attr_map.head._1
      
    val attack_targets = ActionAdventurerDeceive.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    def process : JsCmd = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val cards = card_choose_list.map(CardPool.getByNo(_, gameo.card_list))
      
      if ((card_choose_list.length < 2) ||
          (card_choose_list.length > 3)) {
        return Unblock & Alert("選擇卡片須 2 或 3 張")
      }
      
      val card_attr = cards(0).to_card.cardattr_enum
      cards.foreach { card1 =>
        val card_attr1 = card1.to_card.cardattr_enum
        if (card_attr1 != card_attr)
          return Unblock & Alert("選擇卡片須同屬性")
      }
      
      if (card_choose_list.length == 3)
        attr_str = CardAttrEnum.DARK.toString
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ADVENTURER_DECEIVE.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               //.action_card(card.id.is).action_flags(card.card.is)
                               .action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                               .action_cards(card_choose_list.map(_.toString).mkString(","))
                               .action_flags2(attr_str)
      
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select"  -> card_choose,
         "user_select" -> SHtml.select(attack_targets.map(userentry1 => (userentry1.id.is.toString, userentry1.handle_name.is)), 
                                       Full(attack_targets.head.id.is.toString), x => target_str = x),
         "attr_select" -> SHtml.select(attr_map,
                                Full(attr_str),  x => attr_str = x),
         "deceive"     -> ajaxSubmit("確定", () => process),
         "cancel"     -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
}