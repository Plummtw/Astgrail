package org.plummtw.astgrail.heavy

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


import org.plummtw.astgrail.model._
import org.plummtw.astgrail.enum._
import org.plummtw.astgrail.actor._
import org.plummtw.astgrail.snippet._
import org.plummtw.astgrail.card._
import org.plummtw.astgrail.data._
import org.plummtw.astgrail.util.PlummUtil
import org.plummtw.astgrail.util.CardHelper

object ActionHelper extends Logger {
  def action_list(gameo : GameObject, actioner : UserEntry) : List[ActionData] = {
    val roomphase  =  RoomPhase.get_phase(gameo.room, gameo.roomphases)
    
    if (roomphase.phase_type.is == RoomPhaseEnum.GAMEHALL.toString)
      List(ActionKick, ActionStartGame) // ActionTestAlert
    else if (roomphase.phase_type.is == RoomPhaseEnum.ENDED.toString)
      List()
    else if (roomphase.actioner_id.is != actioner.id.is)
      List()
    else {
      // currentuserentry = CurrentUserEntry_R.get
      val role = actioner.get_role
      
      val result = role.skills(RoomPhaseEnum.get(roomphase.phase_type.is))
      if (role == RoleBrave) {
        warn("RoleBrave : " + roomphase.phase_type.is)
        warn("RoleBrave Skills : " + result.toString)
      }
      result
    }  
  }
  
  def filter_enabled(gameo : GameObject, actioner : UserEntry, actions : List[ActionData]) = actions.filter {x => 
    if (!x.enabled(gameo, actioner)) false
    else {
      var result1 = true
      if ((x.isInstanceOf[UserEntryTargetable]) &&
          (x.asInstanceOf[UserEntryTargetable].targetable_users(gameo, actioner).length == 0))
          result1 = false
      //else if  (x.isInstanceOf[LocationTargetable])
      //  (x.asInstanceOf[LocationTargetable].targetable_locations(room, roomround, roomphase, currentuserentry, userentrys_rr).length != 0)
      if ((x.isInstanceOf[CardTargetable]) &&
          (x.asInstanceOf[CardTargetable].targetable_cards(gameo, actioner).length == 0))
          result1 = false
            
      if ((x.isInstanceOf[CardTargetable2]) &&
          (x.asInstanceOf[CardTargetable2].targetable_cards2(gameo, actioner).length == 0))
          result1 = false
      
      result1
    }
  }
  
  def enabled_action_list(gameo : GameObject, actioner : UserEntry, action_list_in : List[ActionData] = null) : List[ActionData] = {
    val role = actioner.get_role
    val action_list0 = 
      List()
    val action_list1 =
      if (action_list_in != null)
        action_list_in
      else 
        action_list(gameo, actioner)
      
    val normal_actions = List(RoomPhaseEnum.MAIN.toString, RoomPhaseEnum.MAIN_NO_ACTIVATE.toString,
      RoomPhaseEnum.ATTACK.toString, RoomPhaseEnum.MAGIC.toString, RoomPhaseEnum.ATTACK_OR_MAGIC.toString)
    val result = filter_enabled(gameo, actioner, action_list1)
    
    if ((!normal_actions.contains(gameo.roomphase.phase_type.is)) ||
        (gameo.roomphase.actioner_id.is != actioner.id.is))
      result
    else if ((gameo.userentrys.filter(x => (x.get_role == RoleBrave) && (x.target_user.is == actioner.id.is)).length != 0)) {
      warn ("RoleBrave Taunt")
      val activated_skills =
        role.role_skills(RoomPhaseEnum.MAIN) diff role.role_skills(RoomPhaseEnum.MAIN_NO_ACTIVATE)
      val result1 = 
        if ((actioner.get_role == RoleMonk) && (actioner.tapped.is) && (UserEntry.get(actioner.target_user.is, gameo.userentrys).get_role != RoleBrave))
          List(ActionMonk100DragonsRemove, ActionSkipTurn)
        else if ((actioner.get_role == RoleJudicator) && (actioner.yellow_index.is >= RoleJudicator.role_yellow_index_max))
          List(ActionSkipTurn)
        else
          List(ActionAttack, ActionAdventurerDeceive, ActionSkipTurn)
      val result2 =  
        if (gameo.roomphase.phase_type.is == RoomPhaseEnum.MAIN.toString)
          filter_enabled(gameo, actioner, result1 ::: activated_skills)
        else  
          filter_enabled(gameo, actioner, result1)
      result2
    } else if ((actioner.get_role == RoleJudicator) && (actioner.yellow_index.is >= RoleJudicator.role_yellow_index_max)) {
      if (gameo.roomphase.phase_type.is == RoomPhaseEnum.MAIN.toString)
        filter_enabled(gameo, actioner, List(ActionJudicatorBreakRitual, ActionJudicatorFinalJudge, ActionJudicatorRitual))
      else
        List(ActionJudicatorFinalJudge)
    } else if ((actioner.get_role == RoleMonk) && (actioner.tapped.is)) {
      if (gameo.roomphase.phase_type.is == RoomPhaseEnum.MAIN.toString)
        filter_enabled(gameo, actioner, List(ActionAttack, ActionMonk100DragonsRemove, ActionMonkMonkGod))
      else
        filter_enabled(gameo, actioner, List(ActionAttack, ActionMonk100DragonsRemove))
    } else if ((action_list_in == null) && ((result.isEmpty) || (result == List(ActionRefine))) &&
             (gameo.roomphase.phase_type.is == RoomPhaseEnum.MAIN.toString))
      List(ActionCardRenew)
    else if ((actioner.get_role == RoleMagicSword) && (CardPool.in_hand(actioner, gameo.card_list).length > 0) &&
             (gameo.roomphase.phase_type.is == RoomPhaseEnum.MAIN.toString))
      result
    else if ((gameo.roomphase.phase_type.is == RoomPhaseEnum.MAIN.toString) &&
      (result.filter(x => actioner.get_role.skills(RoomPhaseEnum.MAIN_NO_ACTIVATE).contains(x)).length == 0)) {
      val result1 = filter_enabled(gameo, actioner, actioner.get_role.skills(RoomPhaseEnum.MAIN_NO_ACTIVATE) ::: List(ActionPurchase, ActionCombine, ActionRefine))
      if (result1.isEmpty)
        List(ActionCardRenew)
      else
        result1
    } else
      result
  } 
  
  def process_seal(gameo : GameObject, action : Action, card : CardPool, cards : List[CardPool]) : Boolean = {
    val actioner_id   = action.actioner_id.is
    val actioner      = UserEntry.get(actioner_id, gameo.userentrys)
    val action_enum   = MTypeEnum.get_action(action.mtype.is)
    val userentrys    = gameo.userentrys
    //val userentrys_rr = UserEntry.rr(userentrys)
    val roomphase     = gameo.roomphase
    
    cards.foreach(_.discard(gameo))
    
    //if (!gameo.is_delayed_action) 
    val action_cards = 
      if (((action_enum == MTypeEnum.ACTION_BUTTERFLY_POISONPOWDER) ||
           (action_enum == MTypeEnum.ACTION_BUTTERFLY_PILGRIMAGE)) &&
           (action.action_flags2.is == "0")) {
        List()
      } else if (action_enum == MTypeEnum.ACTION_BUTTERFLY_REVERSEFLY) {
        var result = cards.take(2)
        val action_flags2 = action.action_flags2.split(",")
        if (action_flags2.length ==2) {
          if (action_flags2(0) != "0")
            result = result ::: List(cards(2))
          if (action_flags2(1) != "0")
            result = result ::: List(cards(3))
        }
        result
      }
      else cards
        
    val card_revealed  = (if (card == null) List() else List(card)) ::: action_cards
    val card_revealed_attr = card_revealed.map(_.to_card.cardattr_enum)
    var seals_triggered : List[CardPool] = List()
      
    val seals_in_front = CardPool.in_front(actioner, gameo.card_list).filter(_.to_card.has_action(MTypeEnum.ACTION_SEALER_SEAL))
    seals_in_front.foreach { seal =>
      if (card_revealed_attr.contains(seal.to_card.cardattr_enum)) {
        seals_triggered = seals_triggered ::: List(seal)
          
        val seal_talk = Talk.create.roomround_id(gameo.roomround.id.is).mtype(MTypeEnum.RESULT_SEAL.toString)
                           .actioner_id(actioner.id.is).message_flags(seal.to_card.cardattr_enum.toString)
        seal_talk.save
        seal_talk.send(gameo.room)
          
      }
    }
      
    if (seals_triggered.length != 0) {
      val delay_phase = RoomPhase.createFrom(roomphase)
                                 .phase_type(RoomPhaseEnum.DELAYED_ACTION.toString).actioner_id(actioner_id)
                                 .actionee_id(roomphase.actionee_id.is).phase_flags(action.id.is.toString)
      gameo.push_phase(delay_phase)
        //gameo.refresh_roomphase
        //gameo.set_no_pop(true)
        
      seals_triggered.foreach{seal =>
        val process_damage_phase = RoomPhase.createFrom(roomphase)
                                  .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                  .actioner_id(seal.owner_id.is).actionee_id(actioner.id.is)
                                  .power(3) //.card_enum(card.to_card.card_enum.toString)
                                  .last_phase_type("")
                                    //.deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
                                    //.phase_flags(action.id.is.toString)
                                    //.additional(0).additional_flag(RoomAdditionalFlagEnum.DONE.toString)
          //process_damage_phase.save
        RoomPhase.push(gameo, process_damage_phase)
        seal.discard(gameo)
      }

      return true
    }
    false
  }
  
  def action_no_action(gameo: GameObject, action: Action) : Unit = {
    val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
    val phase_type = roomphase.phase_type.is
    val actioner_id   = action.actioner_id.is
    val actioner      = UserEntry.get(actioner_id, gameo.userentrys)
    val userentrys    = gameo.userentrys
        
    if (phase_type == RoomPhaseEnum.ADDITIONALTURN_REACTION.toString) {
      if (actioner.get_role == RoleSwordSaint)
        actioner.add_role_flag(UserEntryRoleFlagEnum.SWORDSAINT_DONE).save
      //gameo.set_addional_turn_checked(true)
    } else if (phase_type == RoomPhaseEnum.LACERATE_REACTION.toString) {
      GameProcessor.process_endure2(gameo, actioner, UserEntry.get(roomphase.actionee_id.is, userentrys))
    } else if (phase_type == RoomPhaseEnum.WATERSHADOW_REACTION.toString) {
      GameProcessor.process_damage(gameo, UserEntry.get(roomphase.actionee_id.is, userentrys), actioner)
    } else if (phase_type == RoomPhaseEnum.SHOCK_REACTION.toString) {
      val roomround = gameo.roomround
      val actionee = UserEntry.get(roomphase.actionee_id.is, userentrys)
      if (!roomround.has_flag(RoomRoundFlagEnum.SHOCKED)) {
        roomround.add_flag(RoomRoundFlagEnum.SHOCKED).save
        val userentryteam = UserEntryTeam.get(actionee.team_id.is, gameo.userentryteams)
        userentryteam.add_gems(1).save
      }
            
      GameProcessor.process_check_heal(gameo, actionee, actioner)
    } else if (phase_type == RoomPhaseEnum.GODCOVER_REACTION.toString) {
      val userentryteam = UserEntryTeam.get(actioner.team_id, gameo.userentryteams)
      userentryteam.moral(userentryteam.moral.is - roomphase.power.is).save
          
      val actionee : UserEntry = UserEntry.get(roomphase.actionee_id.is, userentrys)
      if ((actionee.get_role == RoleRedKnight) && (!actionee.tapped.is) && (roomphase.power.is != 0) &&
          (roomphase.last_phase_type.is != RoomPhaseEnum.WEAKEN_REACTION.toString))
        actionee.tapped(true).save
      
      if ((actioner.get_role == RoleMiko) && (!actioner.tapped.is) &&
          (roomphase.last_phase_type.is != RoomPhaseEnum.WEAKEN_REACTION.toString))
        GameProcessor.check_miko_tap(gameo, actioner)
          
      val soulmages = userentrys.filter(x => (x.team_id.is == actioner.team_id.is) && (x.get_role == RoleSoulMage))
      soulmages.foreach(_.add_yellow_index(roomphase.power.is).save)
        
      RoomActor.sendRoomMessage(gameo.room.id.is, SessionVarSet(room = gameo.room, userentryteams = gameo.userentryteams))
      gameo.add_update(List(ForceUpdateEnum.TEAM_TABLE))
    } else if (phase_type == RoomPhaseEnum.REFLECT_REACTION.toString) {
      //GameProcessor.process_damage(gameo, UserEntry.get(roomphase.actionee_id.is, userentrys), actioner)
    } else if (phase_type == RoomPhaseEnum.SMITE_REACTION.toString) {
      GameProcessor.process_endure2(gameo, actioner, UserEntry.get(roomphase.actionee_id.is, userentrys))
    } else if (phase_type == RoomPhaseEnum.GHOSTS100_REACTION.toString) {
      GameProcessor.process_endure2(gameo, actioner, UserEntry.get(roomphase.actionee_id.is, userentrys))
    }  else if (phase_type == RoomPhaseEnum.POWERBLESS_REACTION.toString) {
      GameProcessor.process_check_heal(gameo, actioner, UserEntry.get(roomphase.actionee_id.is, userentrys))
    } else if (phase_type == RoomPhaseEnum.SWORDKI_REACTION.toString) {
      GameProcessor.process_endure2(gameo, actioner, UserEntry.get(roomphase.actionee_id.is, userentrys))
    } else if (phase_type == RoomPhaseEnum.LIBIDINALWILL_REACTION.toString) {
      actioner.add_role_flag(UserEntryRoleFlagEnum.SWORDEMP_LIBIDINALWILL_DONE).save
    } else if (phase_type == RoomPhaseEnum.MANATIDE_REACTION.toString) {
      actioner.add_role_flag(UserEntryRoleFlagEnum.PRAYER_MANATIDE).save
    } else if (phase_type == RoomPhaseEnum.FASTBLESS_REACTION.toString) {
      actioner.add_user_flag(UserEntryFlagEnum.FASTBLESSED).save
    } else if (phase_type == RoomPhaseEnum.FORBIDDEN_REACTION.toString) {
      if (roomphase.last_phase_type.is == RoomPhaseEnum.ATTACK.toString) {
        GameProcessor.process_endure2(gameo, actioner, UserEntry.get(roomphase.actionee_id.is, userentrys))
      }
          //actioner.add_user_flag(UserEntryFlagEnum.FASTBLESSED).save
    } else if (phase_type == RoomPhaseEnum.BLOODFEAST_REACTION.toString) {
      GameProcessor.process_endure2(gameo, actioner, UserEntry.get(roomphase.actionee_id.is, userentrys))
    } else if (phase_type == RoomPhaseEnum.DISCIPLINE_REACTION.toString) {
      actioner.add_role_flag(UserEntryRoleFlagEnum.REDKNIGHT_DISCIPLINE).save
    } else if (phase_type == RoomPhaseEnum.SOULLINK_REACTION.toString) {
      if (roomphase.phase_flags.is == SoulMageEnum.TOMAGE.toString) {
        GameProcessor.process_damage_pre2(gameo, UserEntry.get(roomphase.actionee_id.is, userentrys), UserEntry.get(actioner.target_user.is, userentrys))
      } else if (roomphase.phase_flags.is == SoulMageEnum.FROMMAGE.toString) {
        GameProcessor.process_damage_pre2(gameo, UserEntry.get(roomphase.actionee_id.is, userentrys), actioner)
      } else 
        warn("Unknown Phase Flags : " + roomphase.phase_flags.is)
    } else if (phase_type == RoomPhaseEnum.PILGRIMAGE_REACTION.toString) {
      GameProcessor.process_damage(gameo, UserEntry.get(roomphase.actionee_id.is, userentrys), actioner)
    } else if ((phase_type == RoomPhaseEnum.POISONPOWDER_REACTION.toString) ||
               (phase_type == RoomPhaseEnum.MIRRORFLOWER_REACTION.toString)) {
      GameProcessor.process_damage_pre(gameo, UserEntry.get(roomphase.actionee_id.is, userentrys), UserEntry.get(roomphase.actionee2_id.is, userentrys))
    } 
  }
  
  def action_attack(gameo: GameObject, action: Action, card : CardPool) : Unit = {
    val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
    val phase_type = roomphase.phase_type.is
    val actioner_id   = action.actioner_id.is
    val actioner      = UserEntry.get(actioner_id, gameo.userentrys)
    val userentrys    = gameo.userentrys
    
    val actionee = UserEntry.get(action.actionee_id, gameo.userentrys)
    val actioner_role = actioner.get_role
        //
    var damage0 = 0
        
    if ((actioner_role == RoleSaintGirl) &&
        (card.to_card.cardattr_enum == CardAttrEnum.WATER)) {
      val icepray_target = UserEntry.get(PlummUtil.parseLong(action.action_flags2.is), gameo.userentrys)
      icepray_target.add_heals(1).save
    } else if ((actioner.get_role == RolePrayer) && (actioner.tapped.is))
      actioner.add_yellow_index(2) //.save
    else if (actioner_role == RoleBrave) {
      actioner.remove_role_flag(UserEntryRoleFlagEnum.BRAVE_ROAR)
              .remove_role_flag(UserEntryRoleFlagEnum.BRAVE_STILLWATER)
      val talkflags = action.action_flags2.is.split(",")
        talkflags.foreach { talkflag =>
          if (talkflag == MTypeEnum.ACTION_BRAVE_ROAR.toString) {
            damage0 += 2
            actioner.lower_yellow_index(1)
                    .add_role_flag(UserEntryRoleFlagEnum.BRAVE_ROAR) //.save
          } else if (talkflag == MTypeEnum.ACTION_BRAVE_STILLWATER.toString) {
            actioner.lower_blue_index(4)
                    .add_role_flag(UserEntryRoleFlagEnum.BRAVE_STILLWATER) //.save
          }
        }
    } else if (actioner_role == RoleRedKnight) {
      if (actioner.hasnt_role_flag(UserEntryRoleFlagEnum.REDKNIGHT_CONTRACT))
        actioner.add_heals(1).add_role_flag(UserEntryRoleFlagEnum.REDKNIGHT_CONTRACT) //.save
    }
        
    val damage1 =
      if (actioner_role == RoleBerserker) {
        var result = 3
        if (card.to_card.has_action(MTypeEnum.ACTION_BERSERKER_BLOODBLADE)) {
          val actionee_cards_in_hand = CardPool.in_hand(actionee, gameo.card_list)
          if (actionee_cards_in_hand.length == 2)
            result += 2
          else if (actionee_cards_in_hand.length == 3)
            result += 1
        }
        result
      } 
      else if ((actioner_role == RoleArcher) && (action.action_flags2.is == MTypeEnum.ACTION_ARCHER_AIM.toString))
        1
      else if ((actioner_role == RoleAssassin) && (actioner.tapped))
        2 + actioner.gems.is + actioner.crystals.is
      else if ((actioner_role == RoleMagicSword) && (actioner.tapped))
        3
      else if (actioner_role == RoleMonk) {
        var result = 2
        if (action.action_flags2.is == MTypeEnum.ACTION_MONK_POWERUP.toString)
          result += 1
        if (actioner.tapped.is)
          result += 2
        result
      }  
      else 2
    roomphase.phase_type(RoomPhaseEnum.ATTACK.toString).power(damage0+damage1)
            //.additional(0).additional_flag(RoomAdditionalFlagEnum.DONE.toString)
             .last_phase_type(RoomPhaseEnum.ATTACK.toString)
             .card_enum(card.to_card.card_enum.toString).save
                 
    if (actioner_role == RoleSwordSaint) {
      //actioner.add_role_flag(UserEntryRoleFlagEnum.ATTACK).save
      if (card.to_card.has_action(MTypeEnum.ACTION_SWORDSAINT_FASTGALE)) {
        val roomround = gameo.roomround
        roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ATTACK.toString).save
      }
    } else if (actioner_role == RoleSaintLance) {
      if (action.action_flags2.is == MTypeEnum.ACTION_SAINTLANCE_SKYLANCE.toString)
        actioner.lower_heals(2).add_role_flag(UserEntryRoleFlagEnum.SAINTLANCE_NOSMITE).save
    } else if (actioner_role == RoleMagicSword) {
      if (action.action_flags2.is == MTypeEnum.ACTION_MAGICSWORD_DARKSTUN.toString)
        actioner.lower_gems(1).add_role_flag(UserEntryRoleFlagEnum.MAGICSWORD_DARKSTUN).add_role_flag(UserEntryRoleFlagEnum.MAGICSWORD_DARKSTUNNED)
      else
        actioner.remove_role_flag(UserEntryRoleFlagEnum.MAGICSWORD_DARKSTUN)
      //actioner.add_role_flag(UserEntryRoleFlagEnum.ATTACK).save
    } else if (actioner_role == RoleSwordEmp) {
      actioner.remove_role_flag(UserEntryRoleFlagEnum.SWORDEMP_ANGELSOUL)
              .remove_role_flag(UserEntryRoleFlagEnum.SWORDEMP_DEVILSOUL)
              .remove_role_flag(UserEntryRoleFlagEnum.SWORDEMP_NOGUARDIAN)
      if ((action.action_flags2.is == MTypeEnum.ACTION_SWORDEMP_ANGELSOUL.toString) ||
          (action.action_flags2.is == MTypeEnum.ACTION_SWORDEMP_DEVILSOUL.toString)) {
         val back_cards = CardPool.in_back(actioner, gameo.card_list)
         back_cards.head.discard(gameo)
         actioner.add_role_flag(UserEntryRoleFlagEnum.SWORDEMP_NOGUARDIAN)
         if (action.action_flags2.is == MTypeEnum.ACTION_SWORDEMP_ANGELSOUL.toString)
           actioner.add_role_flag(UserEntryRoleFlagEnum.SWORDEMP_ANGELSOUL)
         else
           actioner.add_role_flag(UserEntryRoleFlagEnum.SWORDEMP_DEVILSOUL)
       }
       actioner.card_flags(card.id.is.toString)
           
    } else if (actioner_role == RoleMonk) {
      actioner.remove_role_flag(UserEntryRoleFlagEnum.MONK_POWERUP)
              .remove_role_flag(UserEntryRoleFlagEnum.MONK_FIRESOUL)
      if (action.action_flags2.is == MTypeEnum.ACTION_MONK_POWERUP.toString) {
        actioner.add_role_flag(UserEntryRoleFlagEnum.MONK_POWERUP).add_yellow_index(1)
      } else if (action.action_flags2.is == MTypeEnum.ACTION_MONK_FIRESOUL.toString) {
        actioner.add_role_flag(UserEntryRoleFlagEnum.MONK_FIRESOUL)
                .lower_yellow_index(1)
        if (actioner.yellow_index.is != 0) {
          val process_damage_phase1 = RoomPhase.createFrom(roomphase)
                                     .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                     .actioner_id(actioner.id.is).actionee_id(actioner.id.is)
                                     .power(actioner.yellow_index.is) //.card_enum(card.to_card.card_enum.toString)
                                     .last_phase_type("")
          RoomPhase.push(gameo, process_damage_phase1)
        }
      }
      //actioner.save
    } else if (actioner_role == RoleSoulMage) {
      if (action.action_flags2.is == MTypeEnum.ACTION_SOULMAGE_SOULCONVERTY.toString) {
        actioner.add_blue_index(1).lower_yellow_index(1)
      } else if (action.action_flags2.is == MTypeEnum.ACTION_SOULMAGE_SOULCONVERTB.toString) {
        actioner.add_yellow_index(1).lower_blue_index(1)
      }
          //actioner.save
    } 
        
    val taunting_users = gameo.userentrys.filter(x => (x.get_role == RoleBrave) && (x.target_user.is == actioner.id.is))
    taunting_users.foreach(_.target_user(0).save)
      
    actioner.add_role_flag(UserEntryRoleFlagEnum.ATTACK).save
        
    //roomphase.save
         
    GameProcessor.process_attack(gameo, action, card)
  }
  
  def action_reattack(gameo: GameObject, action: Action, card : CardPool) : Unit = {
    val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
    val phase_type = roomphase.phase_type.is
    val actioner_id   = action.actioner_id.is
    val actioner      = UserEntry.get(actioner_id, gameo.userentrys)
    val userentrys    = gameo.userentrys

    val target_player = UserEntry.get(roomphase.actionee_id.is, gameo.userentrys)
    if ((!gameo.is_delayed_action) && (target_player.get_role == RoleArcher) &&
   (roomphase.phase_flags.is == RoomPhaseEnum.ATTACK.toString)) {
      val delay_phase = RoomPhase.createFrom(roomphase)
                                 .phase_type(RoomPhaseEnum.DELAYED_ACTION.toString).actioner_id(actioner.id.is)
                                 //.deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
                                 .actionee_id(roomphase.actionee_id.is).phase_flags(action.id.is.toString)
      RoomPhase.push(gameo, delay_phase)
      //gameo.refresh_roomphase
      val penetrate_phase = RoomPhase.createFrom(roomphase) //.roomround_id(gameo.roomround.id.is)
                                 .phase_type(RoomPhaseEnum.PENETRATE_REACTION.toString).actioner_id(target_player.id.is).actionee_id(actioner.id.is)
                                 //.deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
                                 //.phase_flags(action.id.is.toString)
      RoomPhase.push(gameo, penetrate_phase)
      //gameo.set_no_pop(true)
    } else if ((!gameo.is_delayed_action) && (target_player.has_role_flag(UserEntryRoleFlagEnum.MONK_POWERUP)) &&
          (roomphase.phase_flags.is == RoomPhaseEnum.ATTACK.toString)) {
      val delay_phase = RoomPhase.createFrom(roomphase)
                                 .phase_type(RoomPhaseEnum.DELAYED_ACTION.toString).actioner_id(actioner.id.is)
                                 //.deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
                                 .actionee_id(roomphase.actionee_id.is).phase_flags(action.id.is.toString)
      RoomPhase.push(gameo, delay_phase)

        //target_player.remove_role_flag(UserEntryRoleFlagEnum.MONK_POWERUP).save
        val process_damage_phase1 = RoomPhase.createFrom(roomphase)
                                             .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                             .actioner_id(target_player.id.is).actionee_id(target_player.id.is)
                                             .power(target_player.yellow_index.is) //.card_enum(card.to_card.card_enum.toString)
                                             .last_phase_type("")
        RoomPhase.push(gameo, process_damage_phase1)
    } else if ((!gameo.is_delayed_action) && (target_player.get_role == RoleBrave) && (target_player.crystals.is + target_player.gems.is > 0) &&
          (roomphase.phase_flags.is == RoomPhaseEnum.ATTACK.toString)) {
      val delay_phase = RoomPhase.createFrom(roomphase)
                                 .phase_type(RoomPhaseEnum.DELAYED_ACTION.toString).actioner_id(actioner.id.is)
                                 .actionee_id(roomphase.actionee_id.is).phase_flags(action.id.is.toString)
      RoomPhase.push(gameo, delay_phase)

      val forbidden_phase = RoomPhase.createFrom(roomphase)
                                     .phase_type(RoomPhaseEnum.FORBIDDEN_REACTION.toString)
                                     .actioner_id(target_player.id.is).actionee_id(actioner.id.is)
                                     .last_phase_type("")
      RoomPhase.push(gameo, forbidden_phase)
      if (target_player.has_role_flag(UserEntryRoleFlagEnum.BRAVE_ROAR)) {
        target_player.add_blue_index(1).remove_role_flag(UserEntryRoleFlagEnum.BRAVE_ROAR).save
      }
    } else {
      if ((actioner.get_role == RoleSaintGirl) &&
          (card.to_card.cardattr_enum == CardAttrEnum.WATER)) {
        val icepray_target = UserEntry.get(PlummUtil.parseLong(action.action_flags2.is), gameo.userentrys)
        icepray_target.add_heals(1).save
      } else if (actioner.get_role == RoleMagicSword)
        actioner.remove_role_flag(UserEntryRoleFlagEnum.MAGICSWORD_DARKSTUN).save

      if ((target_player.get_role == RoleSwordEmp) &&
          (roomphase.phase_flags.is == RoomPhaseEnum.ATTACK.toString)) {
        if (target_player.hasnt_role_flag(UserEntryRoleFlagEnum.SWORDEMP_NOGUARDIAN)) {
          if (CardPool.in_back(target_player, gameo.card_list).length < RoleSwordEmp.role_back_cards_max) {
            val card_for_back = CardPool.getById(PlummUtil.parseLong(target_player.card_flags.is), gameo.card_list)
            card_for_back.position(CardPositionEnum.BACK.toString).target_id(target_player.id.is).save
          }
        }
        target_player.add_yellow_index(1)
        if (target_player.has_role_flag(UserEntryRoleFlagEnum.SWORDEMP_ANGELSOUL)) {
          val userentryteam = UserEntryTeam.get(target_player.team_id.is, gameo.userentryteams)
          userentryteam.moral(userentryteam.moral.is + 1).save
        }
        if (target_player.has_role_flag(UserEntryRoleFlagEnum.SWORDEMP_DEVILSOUL)) 
          target_player.add_yellow_index(2)
        target_player.save
      } else if ((target_player.get_role == RoleBrave) &&
                  (roomphase.phase_flags.is == RoomPhaseEnum.ATTACK.toString)) {
        if (target_player.has_role_flag(UserEntryRoleFlagEnum.BRAVE_ROAR)) {
          target_player.add_blue_index(1).remove_role_flag(UserEntryRoleFlagEnum.BRAVE_ROAR).save
        }
      } 

      val damage = 
        if (actioner.get_role == RoleBerserker) 3
        else if ((actioner.get_role == RoleArcher) && (action.action_flags2.is == MTypeEnum.ACTION_ARCHER_AIM.toString))
         1
        else if ((actioner.get_role == RoleMagicSword) && (actioner.tapped))
         3
        else if ((actioner.get_role == RoleMonk) && (actioner.tapped.is)) 
         3
        else 2
      roomphase.phase_type(RoomPhaseEnum.REATTACK.toString).power(damage)
               .last_phase_type(RoomPhaseEnum.REATTACK.toString)
               .card_enum(card.to_card.card_enum.toString).save

      GameProcessor.process_attack(gameo, action, card)
    }
  }
  
  def process_action(gameo: GameObject, action: Action) : Unit = {
    val actioner_id   = action.actioner_id.is
    val actioner      = UserEntry.get(actioner_id, gameo.userentrys)
    val action_enum   = MTypeEnum.get_action(action.mtype.is)
    val userentrys    = gameo.userentrys
    val userentrys_rr = UserEntry.rr(userentrys)
    val roomphase     = gameo.roomphase
    
    //if (gameo.room.stack_index == -1)
    //  gameo.set_first_action(true)
    //gameo.set_delayed_action(is_delayed)
    
    // 卡片之丟棄處理移至這邊 (除了 ACTION_DISCARD)
    val card = 
      try { CardPool.getById(action.action_card.is, gameo.card_list)}
      catch {case e: Exception => null}
    if ((card != null) && (!gameo.is_delayed_action))
      card.discard(gameo)
      
    val cards =
      try { action.action_cards.split(",").toList.map(x => CardPool.getByNo(x.toInt, gameo.card_list)) }
      catch {case e: Exception => List() }
    if ((action_enum != MTypeEnum.ACTION_DISCARD) &&
        (action_enum != MTypeEnum.ACTION_ANGEL_ANGELBLESS_GIVE) &&
        (action_enum != MTypeEnum.ACTION_PRAYER_SHINEBELIEVE) &&
        (action_enum != MTypeEnum.ACTION_BISHOP_HOLYFIELD) &&
        (action_enum != MTypeEnum.ACTION_MONK_MONKGOD) && 
        (action_enum != MTypeEnum.ACTION_SOULMAGE_SOULMIRROR) && 
        (action_enum != MTypeEnum.ACTION_MIKO_REVERSEBLEED) && 
        (action_enum != MTypeEnum.ACTION_MIKO_BLOODCURSE) && 
        (action_enum != MTypeEnum.ACTION_BUTTERFLY_BACKDISCARD) && 
        (!gameo.is_delayed_action)) { 
      if (process_seal(gameo, action, card, cards))
        return
    }
    
    action_enum match {
      case MTypeEnum.ACTION_NO_ACTION =>
        action_no_action(gameo, action)
        // TODO : SwordSaint DONE
        
      case MTypeEnum.ACTION_ATTACK =>
        action_attack(gameo, action, card)
        
        
      case MTypeEnum.ACTION_REATTACK =>
       // val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        //val card = CardPool.getById(action.action_card.is, gameo.card_list)
        //card.discard(gameo)
        action_reattack(gameo, action, card)
        
        
      case MTypeEnum.ACTION_LIGHT =>
        //val card = CardPool.getById(action.action_card.is, gameo.card_list)
        //card.discard(gameo)
        //val roomphase = gameo.roomphase
        //
        if ((roomphase.phase_flags.is == RoomPhaseEnum.ATTACK.toString) ||
           (roomphase.phase_flags.is == RoomPhaseEnum.REATTACK.toString))
          GameProcessor.process_attack_miss(gameo, UserEntry.get(roomphase.actionee_id.is, gameo.userentrys), actioner)
       /*
        val target_player = UserEntry.get(roomphase.actionee_id.is, gameo.userentrys)
        if ((target_player.get_role == RoleArcher) &&
            (roomphase.phase_flags.is == RoomPhaseEnum.ATTACK.toString)) {
          val penetrate_phase = RoomPhase.createFrom(roomphase)
                                     .phase_type(RoomPhaseEnum.PENETRATE_REACTION.toString)
                                     .actioner_id(target_player.id.is).actionee_id(actioner.id.is)
                                     //.deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
                                     //.phase_flags(action.id.is.toString)
          gameo.push_phase(penetrate_phase)
        }        
        else if ((target_player.get_role == RoleSwordEmp) &&
                 (roomphase.phase_flags.is == RoomPhaseEnum.ATTACK.toString)) {
          if (target_player.hasnt_role_flag(UserEntryRoleFlagEnum.SWORDEMP_NOGUARDIAN)) {
            if (CardPool.in_back(target_player, gameo.card_list).length < RoleSwordEmp.role_back_cards_max) {
              val card_for_back = CardPool.getById(PlummUtil.parseLong(target_player.item_flags.is), gameo.card_list)
              card_for_back.position(CardPositionEnum.BACK.toString).target_id(target_player.id.is).save
            }
          }
          target_player.add_yellow_index(1)
          if (target_player.has_role_flag(UserEntryRoleFlagEnum.SWORDEMP_ANGELSOUL)) {
            val userentryteam = UserEntryTeam.get(target_player.team_id.is, gameo.userentryteams)
            userentryteam.moral(userentryteam.moral.is + 1).save
          }
          if (target_player.has_role_flag(UserEntryRoleFlagEnum.SWORDEMP_DEVILSOUL)) 
            target_player.add_yellow_index(2)
          target_player.save
        }
        */
        
        if (roomphase.phase_type.is == RoomPhaseEnum.MBOLT_REACTION.toString) {
          val mbolt_userentrys = userentrys_rr.filter(_.has_user_flag(UserEntryFlagEnum.MBOLT))
          mbolt_userentrys.foreach(_.remove_user_flag(UserEntryFlagEnum.MBOLT).save)
          gameo.roomround.remove_flag(RoomRoundFlagEnum.MBOLT_REVERSE)
        }
        
        gameo.add_update(List(ForceUpdateEnum.TEAM_TABLE))
      case MTypeEnum.ACTION_HEAL =>
        //val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        val heals     = action.action_flags.is.toInt
        actioner.heals(math.max(0, actioner.heals.is - heals)).save
        roomphase.power(roomphase.power.is - heals).save
        GameProcessor.process_post_heal(gameo, UserEntry.get(roomphase.actionee_id.is, userentrys), actioner)
        
      case MTypeEnum.ACTION_MAGIC_POISON =>
        //val card = CardPool.getById(action.action_card.is, gameo.card_list)
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        card.to_front(gameo, actioner_id, actionee_id)
        
        val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                   .phase_subtype(CardMagicEnum.POISON.toString)
                   //.additional(0).additional_flag(RoomAdditionalFlagEnum.DONE.toString)
                   .power(1).card_enum(card.to_card.card_enum.toString)
        roomphase.save
        if (actioner.get_role == RoleSealer) {        
          val roomround = gameo.roomround
          roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ATTACK.toString).save
        } else if (actioner.get_role == RoleNecromancer) {
          actioner.add_role_flag(UserEntryRoleFlagEnum.NECROMANCER_MAGIC)
        } else if (actioner.get_role == RolePrayer) {
          actioner.add_role_flag(UserEntryRoleFlagEnum.PRAYER_MAGIC)
        } else if ((actioner.get_role == RoleMonk) && (actioner.yellow_index.is < RoleMonk.role_yellow_index_max)) {
          val new_phase = RoomPhase.createFrom(roomphase) //.roomround_id(gameo.roomround.id.is)
                               .card_enum(roomphase.card_enum.is)
                               //.phase_flags(roomphase.phase_flags.is)
                               .phase_type(RoomPhaseEnum.KISHOT_REACTION.toString)
                               .actioner_id(actioner.id.is).actionee_id(actionee.id.is)
                               //.deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
          RoomPhase.push(gameo, new_phase)
        }
        actioner.add_role_flag(UserEntryRoleFlagEnum.MAGIC).save
        
      case MTypeEnum.ACTION_MAGIC_WEAKEN =>
        //val card = CardPool.getById(action.action_card.is, gameo.card_list)
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        card.target_id(actionee_id).position(CardPositionEnum.FRONT.toString).save
        
        val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(CardMagicEnum.WEAKEN.toString)
                 //.additional(0).additional_flag(RoomAdditionalFlagEnum.DONE.toString)
                 .card_enum(card.to_card.card_enum.toString)
        roomphase.save
        if (actioner.get_role == RoleSealer) {        
          val roomround = gameo.roomround
          roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ATTACK.toString).save
        } else if (actioner.get_role == RoleNecromancer) {
          actioner.add_role_flag(UserEntryRoleFlagEnum.NECROMANCER_MAGIC)
        } else if (actioner.get_role == RolePrayer) {
          actioner.add_role_flag(UserEntryRoleFlagEnum.PRAYER_MAGIC)
        } else if ((actioner.get_role == RoleMonk) && (actioner.yellow_index.is < RoleMonk.role_yellow_index_max)) {
          val new_phase = RoomPhase.createFrom(roomphase) //.roomround_id(gameo.roomround.id.is)
                               .card_enum(roomphase.card_enum.is)
                               //.phase_flags(roomphase.phase_flags.is)
                               .phase_type(RoomPhaseEnum.KISHOT_REACTION.toString)
                               .actioner_id(actioner.id.is).actionee_id(actionee.id.is)
                               //.deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
          RoomPhase.push(gameo, new_phase)
        }
        actioner.add_role_flag(UserEntryRoleFlagEnum.MAGIC).save
        
      case MTypeEnum.ACTION_MAGIC_SHIELD =>
        //val card = CardPool.getById(action.action_card.is, gameo.card_list)
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        card.to_front(gameo, actioner_id, actionee_id)
        
        val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                   .phase_subtype(CardMagicEnum.SHIELD.toString)
                   //.additional(0).additional_flag(RoomAdditionalFlagEnum.DONE.toString)
                   .card_enum(card.to_card.card_enum.toString)
        roomphase.save
        if (actioner.get_role == RoleSealer) {        
          val roomround = gameo.roomround
          roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ATTACK.toString).save
        } else if (actioner.get_role == RoleAngel) {
          val bond_actionee = UserEntry.get(PlummUtil.parseLong(action.action_flags2.is), userentrys)
          bond_actionee.add_heals(1).save
        } else if (actioner.get_role == RoleNecromancer) {
          actioner.add_role_flag(UserEntryRoleFlagEnum.NECROMANCER_MAGIC)
        } else if (actioner.get_role == RolePrayer) {
          actioner.add_role_flag(UserEntryRoleFlagEnum.PRAYER_MAGIC)
        } else if ((actioner.get_role == RoleMonk) && (actioner.yellow_index.is < RoleMonk.role_yellow_index_max)) {
          val new_phase = RoomPhase.createFrom(roomphase) //.roomround_id(gameo.roomround.id.is)
                               .card_enum(roomphase.card_enum.is)
                               //.phase_flags(roomphase.phase_flags.is)
                               .phase_type(RoomPhaseEnum.KISHOT_REACTION.toString)
                               .actioner_id(actioner.id.is).actionee_id(actionee.id.is)
                               //.deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
          RoomPhase.push(gameo, new_phase)
        }
        actioner.add_role_flag(UserEntryRoleFlagEnum.MAGIC).save
        
      case MTypeEnum.ACTION_MAGIC_MBOLT  =>
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)        //val card = CardPool.getById(action.action_card.is, gameo.card_list)
        //card.discard(gameo)
        val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        if (roomphase.phase_type.is == RoomPhaseEnum.MBOLT_REACTION.toString) 
          roomphase.power(roomphase.power.is + 1).save
        else {             
          roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                   .phase_subtype(CardMagicEnum.MBOLT.toString)
                   //.additional(0).additional_flag(RoomAdditionalFlagEnum.DONE.toString)
                   .power(2).card_enum(card.to_card.card_enum.toString)
          roomphase.save
          if (actioner.get_role == RoleSealer) {
            val roomround = gameo.roomround
            roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ATTACK.toString).save
          } else if ((actioner.get_role == RoleMage) && (action.action_flags2.is == RoomRoundFlagEnum.MBOLT_REVERSE.toString)) {
            val roomround = gameo.roomround
            roomround.add_flag(RoomRoundFlagEnum.MBOLT_REVERSE).save
          } else if (actioner.get_role == RoleNecromancer) {
            actioner.add_role_flag(UserEntryRoleFlagEnum.NECROMANCER_MAGIC)
          } else if (actioner.get_role == RolePrayer) {
            actioner.add_role_flag(UserEntryRoleFlagEnum.PRAYER_MAGIC)
          } else if ((actioner.get_role == RoleMonk) && (actioner.yellow_index.is < RoleMonk.role_yellow_index_max)) {
            val new_phase = RoomPhase.createFrom(roomphase) //.roomround_id(gameo.roomround.id.is)
                               .card_enum(roomphase.card_enum.is)
                               //.phase_flags(roomphase.phase_flags.is)
                               .phase_type(RoomPhaseEnum.KISHOT_REACTION.toString)
                               .actioner_id(actioner.id.is).actionee_id(actionee.id.is)
                               //.deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
            RoomPhase.push(gameo, new_phase)
          }
          actioner.add_user_flag(UserEntryFlagEnum.MBOLT).add_role_flag(UserEntryRoleFlagEnum.MAGIC).save
        }
        
        var is_save_all = false
        
        val mbolt_userentrys = userentrys_rr.filter(_.has_user_flag(UserEntryFlagEnum.MBOLT))
        if (mbolt_userentrys.length == userentrys_rr.length) {
          mbolt_userentrys.foreach(_.remove_user_flag(UserEntryFlagEnum.MBOLT))
          is_save_all = true
        }
        
        actionee.add_user_flag(UserEntryFlagEnum.MBOLT)
        if (is_save_all)
          mbolt_userentrys.foreach(_.save)
        else
          actionee.save
         
        GameProcessor.process_mbolt(gameo, action, card)
        
      case MTypeEnum.ACTION_ENDUREATTACK =>
        // ENDURE_ATTACK
        //val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        val actionee = UserEntry.get(roomphase.actionee_id.is, gameo.userentrys)
        //val card = CardEnum.get_card(roomphase.card_enum.is.toString)
        
        GameProcessor.process_endure(gameo, actionee, actioner)
        
      case MTypeEnum.ACTION_ENDUREMAGIC =>  
        // ENDURE_MBOLT
        //val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        if (roomphase.phase_type.is == RoomPhaseEnum.MBOLT_REACTION.toString) {
          val mbolt_userentrys = userentrys_rr.filter(_.has_user_flag(UserEntryFlagEnum.MBOLT))
          mbolt_userentrys.foreach(_.remove_user_flag(UserEntryFlagEnum.MBOLT).save)
          //val card = CardEnum.get_card(roomphase.card_enum.is.toString)
          val roomround = gameo.roomround
          roomround.remove_flag(RoomRoundFlagEnum.MBOLT_REVERSE).save
          
          GameProcessor.process_endure(gameo, UserEntry.get(roomphase.actionee_id.is, userentrys), actioner)
        } else if (roomphase.phase_type.is == RoomPhaseEnum.WEAKEN_REACTION.toString) {
          val cards_in_front = CardPool.in_front(actioner, gameo.card_list)
          val weakens_in_front = cards_in_front.filter(_.to_card.cardmagic_enum == CardMagicEnum.WEAKEN)
          weakens_in_front.foreach(_.discard(gameo))
          //GameProcessor.process_check_heal(gameo, UserEntry.get(roomphase.actionee_id.is, userentrys), actioner)
          
          val weaken_talk = Talk.create.roomround_id(gameo.roomround.id.is).mtype(MTypeEnum.RESULT_WEAKEN.toString)
                                       .actioner_id(actioner.id.is).message_flags(roomphase.power.is.toString)
          weaken_talk.save
          weaken_talk.send(gameo.room)
          
          GameProcessor.process_drawcard(gameo, actioner, roomphase.power.is)
        } else if (roomphase.phase_type.is == RoomPhaseEnum.FIVESEAL_REACTION.toString) {
          val sealers = gameo.userentrys.filter(x => (x.get_role == RoleSealer) && (x.target_user.is == actioner.id.is))
          sealers.foreach(_.target_user(0).save)
          
          val weaken_talk = Talk.create.roomround_id(gameo.roomround.id.is).mtype(MTypeEnum.RESULT_WEAKEN.toString)
                                       .actioner_id(actioner.id.is).message_flags(roomphase.power.is.toString)
          weaken_talk.save
          weaken_talk.send(gameo.room)
          //val weakens_in_front = cards_in_front.filter(_.to_card.cardmagic_enum == CardMagicEnum.WEAKEN)
          //weakens_in_front.foreach(_.discard(gameo))
          //GameProcessor.process_check_heal(gameo, UserEntry.get(roomphase.actionee_id.is, userentrys), actioner)
          GameProcessor.process_drawcard(gameo, actioner, roomphase.power.is)
        }
        
      case MTypeEnum.ACTION_PURCHASE =>
        //val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        //roomphase.additional_flag(RoomAdditionalFlagEnum.DONE.toString).save
        val userentryteam = UserEntryTeam.get(actioner.team_id.is, gameo.userentryteams)
        if (userentryteam.gems.is + userentryteam.crystals.is < 5) {
          if (userentryteam.gems.is + userentryteam.crystals.is < 4) {
            if (actioner.get_role == RoleAdventurer)
              userentryteam.gems(userentryteam.gems.is + 2).save
            else
              userentryteam.gems(userentryteam.gems.is + 1).crystals(userentryteam.crystals.is + 1).save
          } else
            userentryteam.gems(userentryteam.gems.is + 1).save
        }
        for (i <- 0 until 3) {
          val card = CardHelper.draw_card(gameo)
          card.owner_id(actioner_id).position(CardPositionEnum.HAND.toString).save
        }
        //gameo.room.save
        
        if (actioner.get_role == RoleBishop)
          actioner.add_heals(1)
        
        actioner.add_role_flag(UserEntryRoleFlagEnum.SPECIAL).save
        
        RoomActor.sendRoomMessage(gameo.room.id.is, SessionVarSet(room = gameo.room, userentryteams = gameo.userentryteams))
        gameo.add_update(List(ForceUpdateEnum.TEAM_TABLE))
        
      case MTypeEnum.ACTION_COMBINE =>
        //val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        //roomphase.additional_flag(RoomAdditionalFlagEnum.DONE.toString).save
        val userentryteam = UserEntryTeam.get(actioner.team_id.is, gameo.userentryteams)
        val action_flags  = action.action_flags.is
        val gem_count     = action_flags.count(_.toString == GameEnum.GEM1.toString)
        val crystal_count = action_flags.count(_.toString == GameEnum.CRYSTAL1.toString)
        assert(gem_count + crystal_count == 3)
        
        userentryteam.gems(userentryteam.gems.is - gem_count)
        userentryteam.crystals(userentryteam.crystals.is - crystal_count)
        userentryteam.grails(userentryteam.grails.is + 1)
        userentryteam.save
        
        for (i <- 0 until 3) {
          val card = CardHelper.draw_card(gameo)
          card.owner_id(actioner_id).position(CardPositionEnum.HAND.toString).save
        }
        
        // 其他隊扣士氣
        val enemyteam = gameo.userentryteams.filterNot(_ == userentryteam)(0)
        val enemyteam_angels = userentrys.filter(x => (x.team_id.is == enemyteam.id.is) &&
                                                      (x.get_role == RoleAngel))
        //if ((enemyteam_angels.length != 0) && (enemyteam_angels(0).gems.is + enemyteam_angels(0).crystals.is > 0)) {
        //  val godcover_phase = RoomPhase.createFrom(roomphase).phase_type(RoomPhaseEnum.GODCOVER_REACTION.toString)
        //                       .actioner_id(enemyteam_angels(0).id.is).power(1)
        //  gameo.push_phase(godcover_phase)
        //} else {
          enemyteam.moral(enemyteam.moral.is - 1).save
          RoomActor.sendRoomMessage(gameo.room.id.is, SessionVarSet(room = gameo.room, userentryteams = gameo.userentryteams, card_list = gameo.card_list))
          gameo.add_update(List(ForceUpdateEnum.TEAM_TABLE))
        //}

        //gameo.room.save
        if (actioner.get_role == RoleBishop)
          actioner.add_heals(1)
        actioner.add_role_flag(UserEntryRoleFlagEnum.SPECIAL).save
        
      case MTypeEnum.ACTION_REFINE =>
        //val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        //roomphase.additional_flag(RoomAdditionalFlagEnum.DONE.toString).save
        val userentryteam = UserEntryTeam.get(actioner.team_id.is, gameo.userentryteams)
        val action_flags  = action.action_flags.is
        val gem_count     = action_flags.count(_.toString == GameEnum.GEM1.toString)
        val crystal_count = action_flags.count(_.toString == GameEnum.CRYSTAL1.toString)
        //assert(gem_count + crystal_count <= 2)
        
        val target_user =
          if (actioner.get_role == RoleAdventurer) UserEntry.get(action.actionee_id.is, gameo.userentrys)
          else actioner
        
        userentryteam.gems(userentryteam.gems.is - gem_count)
        target_user.gems(target_user.gems.is + gem_count)
        userentryteam.crystals(userentryteam.crystals.is - crystal_count)
        target_user.crystals(target_user.crystals.is + crystal_count)
        
        if (actioner.get_role == RoleBishop)
          actioner.add_heals(1)
        actioner.add_role_flag(UserEntryRoleFlagEnum.SPECIAL).save
        
        target_user.save
        userentryteam.save
        
        RoomActor.sendRoomMessage(gameo.room.id.is, SessionVarSet(room = gameo.room, userentryteams = gameo.userentryteams))
        gameo.add_update(List(ForceUpdateEnum.TEAM_TABLE))
        
      case MTypeEnum.ACTION_DISCARD =>
        val roomphase = gameo.roomphase
        val userentryteam = UserEntryTeam.get(actioner.team_id.is, gameo.userentryteams)
        //val cards = action.action_cards.split(",").toList.map(x => CardPool.getByNo(x.toInt, gameo.card_list))
        
        var moral_hit = 0
        cards.foreach { card =>
          card.discard(gameo)
          //userentryteam.moral(userentryteam.moral.is - 1)
          moral_hit += 1
        }
        
        if ((actioner.get_role == RoleRedKnight) && (actioner.tapped.is) &&
            (roomphase.last_phase_type.is != RoomPhaseEnum.WEAKEN_REACTION.toString))
          moral_hit = 0
        
        val userteam_angels = userentrys.filter(x => (x.team_id.is == userentryteam.id.is) &&
                                                     (x.get_role == RoleAngel))
        if ((moral_hit != 0 ) && (userteam_angels.length != 0) && 
            (roomphase.last_phase_type.is != RoomPhaseEnum.ATTACK.toString) &&
            (roomphase.last_phase_type.is != RoomPhaseEnum.REATTACK.toString) &&
            (roomphase.last_phase_type.is != RoomPhaseEnum.WEAKEN_REACTION.toString) &&
            //(roomphase.last_phase_type.is != RoomPhaseEnum.FIVESEAL_REACTION.toString) &&
            (userteam_angels(0).gems.is + userteam_angels(0).crystals.is > 0)) {
          val godcover_phase = RoomPhase.createFrom(roomphase).phase_type(RoomPhaseEnum.GODCOVER_REACTION.toString)
                                       .actioner_id(userteam_angels(0).id.is).actionee_id(actioner.id.is).power(moral_hit)
          gameo.push_phase(godcover_phase)
        } else {
          if (moral_hit != 0) {
            userentryteam.moral(userentryteam.moral.is - moral_hit).save
          
            if ((actioner.get_role == RoleRedKnight) && (!actioner.tapped.is) &&
                (roomphase.last_phase_type.is != RoomPhaseEnum.WEAKEN_REACTION.toString))
              actioner.tapped(true).save
        
            if ((actioner.get_role == RoleMiko) && (!actioner.tapped.is) &&
                (roomphase.last_phase_type.is != RoomPhaseEnum.WEAKEN_REACTION.toString))
                  GameProcessor.check_miko_tap(gameo, actioner)
          
            val soulmages = userentrys.filter(x => (x.team_id.is == actioner.team_id.is) && (x.get_role == RoleSoulMage))
            soulmages.foreach(_.add_yellow_index(moral_hit).save)
        
            RoomActor.sendRoomMessage(gameo.room.id.is, SessionVarSet(room = gameo.room, userentryteams = gameo.userentryteams))
            gameo.add_update(List(ForceUpdateEnum.TEAM_TABLE))
          }
        }
      case MTypeEnum.ACTION_CARD_RENEW =>  
        GameProcessor.process_drawcard(gameo, actioner, cards.length)
        //val hand_cards = CardPool.in_hand(actioner, gameo.card_list)
        //val hand_card_number = hand_cards.length
        //hand_cards.foreach(_.discard(gameo))
        
        //for (i <- 0 until hand_card_number) {
        //  val card = CardHelper.draw_card(gameo)
        //  card.owner_id(actioner.id.is).position(CardPositionEnum.HAND.toString).save
        //}
        //gameo.room.save
        
        //RoomActor.sendRoomMessage(gameo.room.id.is, SessionVarSet(room = gameo.room, userentryteams = gameo.userentryteams))
        //gameo.add_update(List(ForceUpdateEnum.TEAM_TABLE))
        
      case MTypeEnum.ACTION_SKIPTURN => 
        val roomphase = gameo.roomphase
        if (roomphase.phase_type.is == RoomPhaseEnum.WEAKEN_REACTION.toString) {
          val cards_in_front = CardPool.in_front(actioner, gameo.card_list)
          val weakens_in_front = cards_in_front.filter(_.to_card.cardmagic_enum == CardMagicEnum.WEAKEN)
          weakens_in_front.foreach(_.discard(gameo))
          //GameProcessor.process_damage_pre(gameo, UserEntry.get(roomphase.target_player.is, userentrys), actioner)
        } else if (roomphase.phase_type.is == RoomPhaseEnum.FIVESEAL_REACTION.toString) {
          val sealers = gameo.userentrys.filter(x => (x.get_role == RoleSealer) && (x.target_user.is == actioner.id.is))
          sealers.foreach(_.target_user(0).save)
          //val weakens_in_front = cards_in_front.filter(_.to_card.cardmagic_enum == CardMagicEnum.WEAKEN)
          //weakens_in_front.foreach(_.discard(gameo))
        } else if (roomphase.phase_type.is == RoomPhaseEnum.MAIN.toString) {
          val taunting_users = gameo.userentrys.filter(x => (x.get_role == RoleBrave) && (x.target_user.is == actioner.id.is))
          taunting_users.foreach(_.target_user(0).save)
        }
        //val cards_in_front = CardPool.in_front(actioner, gameo.card_list)
        //val weakens_in_front = cards_in_front.filter(_.to_card.cardmagic_enum == CardMagicEnum.WEAKEN)
        //weakens_in_front.foreach(_.discard(gameo))
        
        //val main_phase = RoomPhase.last(gameo.roomphases)
        //main_phase.additional_flag(RoomAdditionalFlagEnum.DONE.toString).save
        gameo.room.stack_index(-1)
        //GameProcessor.next_player(gameo)
        //return
        
      case MTypeEnum.ACTION_SWORDSAINT_COMBO =>
        //val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        actioner.add_role_flag(UserEntryRoleFlagEnum.SWORDSAINT_COMBO)
                .add_role_flag(UserEntryRoleFlagEnum.SWORDSAINT_DONE).save
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ATTACK_AIR.toString).save
        
      case MTypeEnum.ACTION_SWORDSAINT_SWORDSHADOW =>
        //val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        if (actioner.crystals.is == 0)
          actioner.gems(actioner.gems.is - 1)
        else
          actioner.crystals(actioner.crystals.is - 1)
        actioner.add_role_flag(UserEntryRoleFlagEnum.SWORDSAINT_SWORDSHADOW)
                .add_role_flag(UserEntryRoleFlagEnum.SWORDSAINT_DONE).save
        
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ATTACK.toString).save
        
      case MTypeEnum.ACTION_BERSERKER_LACERATE =>
        val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        actioner.gems(actioner.gems.is - 1).save
        roomphase.power(roomphase.power.is + 2).save
        GameProcessor.process_endure2(gameo, actioner, UserEntry.get(roomphase.actionee_id.is, userentrys))
        
      case MTypeEnum.ACTION_ARCHER_TRAP =>
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        //val card = CardPool.getById(action.action_card.is, gameo.card_list)
        //val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString)
                 //.additional(0).additional_flag(RoomAdditionalFlagEnum.DONE.toString)
                 .power(2).card_enum(card.to_card.card_enum.toString).save
        //card.discard(gameo)
        GameProcessor.process_endure(gameo, actioner, actionee, true)
                                     
      case MTypeEnum.ACTION_ARCHER_SNIPE =>
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        //val card = CardPool.getById(action.action_card.is, gameo.card_list)
        //val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        roomphase.phase_type(RoomPhaseEnum.MAIN_NO_ACTIVATE.toString)
                 .phase_subtype(action.mtype.is.toString)
                 //.additional(1).additional_flag(RoomAdditionalFlagEnum.BOTH.toString)
                 .power(2).save
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ATTACK.toString).save
        //card.discard(gameo)
        if (actioner.crystals.is == 0)
          actioner.gems(actioner.gems.is - 1)
        else
          actioner.crystals(actioner.crystals.is - 1)
        actioner.save
        //GameProcessor.process_endure(gameo, actioner, actionee, true)
        val draw_number = math.max(0, 5  - CardPool.in_hand(actionee, gameo.card_list).length)
        GameProcessor.process_drawcard(gameo, actionee, draw_number)
        
      case MTypeEnum.ACTION_ARCHER_PENETRATE =>
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        //val card = CardPool.getById(action.action_card.is, gameo.card_list)
        //val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString)
                 .last_phase_type("")
                 //.additional(0).additional_flag(RoomAdditionalFlagEnum.DONE.toString)
                 .power(2).card_enum(card.to_card.card_enum.toString).save
        //card.discard(gameo)
        GameProcessor.process_check_heal(gameo, actioner, actionee) //, true)
        
      case MTypeEnum.ACTION_ASSASSIN_WATERSHADOW =>
        val roomphase = gameo.roomphase
        val actionee = UserEntry.get(roomphase.actionee_id, gameo.userentrys)
        //val cards = action.action_cards.split(",").toList.map(x => CardPool.getByNo(x.toInt, gameo.card_list))
        //roomphase.power(math.max(0, roomphase.power.is - cards.length)).save
        //cards.foreach(_.discard(gameo))
        
        //RoomActor.sendRoomMessage(gameo.room.id.is, SessionVarSet(room = gameo.room, userentrys = gameo.userentrys))
        //gameo.add_update(List(ForceUpdateEnum.USER_TABLE))
        
        GameProcessor.process_damage(gameo, actionee, actioner)
        
      case MTypeEnum.ACTION_ASSASSIN_SNEAK =>
        //val roomphase = gameo.roomphase
        actioner.tapped(true).hand_max(actioner.hand_max.is - 1).gems(actioner.gems.is - 1).save
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ACTIVATE.toString).save
        
        //gameo.add_update(List(ForceUpdateEnum.USER_TABLE))
        GameProcessor.check_hand_max(gameo, actioner)
        
      case MTypeEnum.ACTION_ANGEL_WINDPURIFY =>
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString)
                 .card_enum(card.to_card.card_enum.toString).save
        val card2 = 
          try { CardPool.getById(action.action_card2.is, gameo.card_list)}
          catch {case e: Exception => null}
        card2.discard(gameo)
        
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        actionee.add_heals(1).save
      case MTypeEnum.ACTION_ANGEL_ANGELBLESS =>
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString)
                 .card_enum(card.to_card.card_enum.toString).save
        val actionees = 
          try { action.action_flags2.split(",").toList.map(x => UserEntry.get(x.toLong, gameo.userentrys)) }
          catch {case e: Exception => List() }
        actionees.foreach { actionee =>
          if (CardPool.in_hand(actionee, gameo.card_list).length != 0) {
            val give_phase = RoomPhase.createFrom(roomphase).phase_type(RoomPhaseEnum.ANGELBLESS_REACTION.toString)
                                        .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
                                        .power(if (actionees.length == 1) 2 else 1)
            gameo.push_phase(give_phase)
          }
        }
      case MTypeEnum.ACTION_ANGEL_ANGELBLESS_GIVE =>
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        cards.foreach { card1 =>
          card1.owner_id(actionee_id).position(CardPositionEnum.HAND.toString).save
        }
        if ((actioner.get_role == RoleMiko) && (actioner.tapped.is) &&
            (CardPool.in_hand(actioner, gameo.card_list).length < 3))
          GameProcessor.check_miko_untap(gameo, actioner)
        GameProcessor.check_hand_max(gameo, actionee)
      case MTypeEnum.ACTION_ANGEL_ANGELSONG =>
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString)
                 //.card_enum(card.to_card.card_enum.toString)
                 .save
        actioner.lower_crystals(1).save
        val card2 = 
          try { CardPool.getById(action.action_card2.is, gameo.card_list)}
          catch {case e: Exception => null}
        card2.discard(gameo)
        
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        actionee.add_heals(1).save
      case MTypeEnum.ACTION_ANGEL_GODCOVER =>
        val crystal_used = 
          try{ action.action_flags.is.toInt }
          catch {case e : Exception => 0}
        actioner.lower_crystals(crystal_used).save
        val moral_hit = roomphase.power.is - crystal_used

        if (moral_hit > 0) {
          val userentryteam = UserEntryTeam.get(actioner.team_id.is, gameo.userentryteams)
          userentryteam.moral(userentryteam.moral.is - moral_hit).save
          
          val actionee : UserEntry = UserEntry.get(roomphase.actionee_id.is, userentrys)
          if ((actionee.get_role == RoleRedKnight) && (!actionee.tapped.is) &&
              (roomphase.last_phase_type.is != RoomPhaseEnum.WEAKEN_REACTION.toString))
            actionee.tapped(true).save
      
          if ((actionee.get_role == RoleMiko) && (!actionee.tapped.is) &&
              (roomphase.last_phase_type.is != RoomPhaseEnum.WEAKEN_REACTION.toString))
            GameProcessor.check_miko_tap(gameo, actionee)
     
          val soulmages = userentrys.filter(x => (x.team_id.is == actioner.team_id.is) && (x.get_role == RoleSoulMage))
          soulmages.foreach(_.add_yellow_index(moral_hit).save)
        
          RoomActor.sendRoomMessage(gameo.room.id.is, SessionVarSet(room = gameo.room, userentryteams = gameo.userentryteams))
          gameo.add_update(List(ForceUpdateEnum.TEAM_TABLE))
        }
      case MTypeEnum.ACTION_SAINTGIRL_HEAL =>
        //roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
        //         .phase_subtype(action.mtype.is.toString)
        //         .card_enum(card.to_card.card_enum.toString).save
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        //val card = CardPool.getById(action.action_card.is, gameo.card_list)
        //val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString)
                 //.additional_flag(RoomAdditionalFlagEnum.DONE.toString)
                 .card_enum(card.to_card.card_enum.toString).save
        //card.discard(gameo)
        actionee.add_heals(2).save
        //gameo.add_update(List(ForceUpdateEnum.USER_TABLE))
        
      case MTypeEnum.ACTION_SAINTGIRL_HEALLIGHT =>
        //roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
        //         .phase_subtype(action.mtype.is.toString)
        //         .card_enum(card.to_card.card_enum.toString).save
        val actionees = 
          try { action.action_flags2.split(",").toList.map(x => UserEntry.get(x.toLong, gameo.userentrys)) }
          catch {case e: Exception => List() }
        
        //val card = CardPool.getById(action.action_card.is, gameo.card_list)
        //val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString)
                 //.additional_flag(RoomAdditionalFlagEnum.DONE.toString)
                 .card_enum(card.to_card.card_enum.toString).save
        //card.discard(gameo)
        actionees.foreach { actionee =>
          actionee.add_heals(1).save
        }
        //gameo.add_update(List(ForceUpdateEnum.USER_TABLE))
        
      case MTypeEnum.ACTION_SAINTGIRL_COMPASSION =>
        actioner.tapped(true).fixed_hand_max(7).lower_gems(1).save
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ACTIVATE.toString).save
        //gameo.add_update(List(ForceUpdateEnum.USER_TABLE))
      
      case MTypeEnum.ACTION_SAINTGIRL_HOLYHEAL =>
        actioner.lower_crystals(1).add_role_flag(UserEntryRoleFlagEnum.SAINTGIRL_HOLYHEAL).save
        val actionees = 
          try { action.action_flags2.split(",").toList.map(x => UserEntry.get(x.toLong, gameo.userentrys)) }
          catch {case e: Exception => List() }
        
        //val card = CardPool.getById(action.action_card.is, gameo.card_list)
        //val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString)
                 //.additional(1).additional_flag(RoomAdditionalFlagEnum.BOTH.toString)
                 //.card_enum(card.to_card.card_enum.toString)
                 .save
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.BOTH.toString).save
        //card.discard(gameo)
        actionees.foreach { actionee =>
          actionee.add_heals(1).save
        }
        
        //gameo.add_update(List(ForceUpdateEnum.USER_TABLE))  
        
      case MTypeEnum.ACTION_SEALER_SEAL =>
        
        val actionee_id = action.actionee_id.is
        //val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        //val card = CardPool.getById(action.action_card.is, gameo.card_list)
        //val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString)
                 //.additional(1).additional_flag(RoomAdditionalFlagEnum.ATTACK.toString)
                 .card_enum(card.to_card.card_enum.toString).save
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ATTACK.toString).save
        card.to_front(gameo, actioner_id, actionee_id)
        
        //gameo.add_update(List(ForceUpdateEnum.USER_TABLE))
        
      case MTypeEnum.ACTION_SEALER_FIVESEAL =>
        val actionee_id = action.actionee_id.is
        actioner.target_user(actionee_id).lower_crystals(1).save
        //val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        //val card = CardPool.getById(action.action_card.is, gameo.card_list)
        //val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString)
                 //.additional(1).additional_flag(RoomAdditionalFlagEnum.ATTACK.toString)
                 //.card_enum(card.to_card.card_enum.toString)
                 .save
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ATTACK.toString).save         
        //gameo.add_update(List(ForceUpdateEnum.USER_TABLE))
        
      case MTypeEnum.ACTION_SEALER_BREAKSEAL =>
        actioner.lower_crystals(1).save
        //val actionee_id = action.actionee_id.is
        //val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        //val card = CardPool.getById(action.action_card.is, gameo.card_list)
        //val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString)
                 //.additional(1).additional_flag(RoomAdditionalFlagEnum.ATTACK.toString)
                 //.card_enum(card.to_card.card_enum.toString)
                 .save
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ATTACK.toString).save         
        val card2 = 
          try { CardPool.getById(action.action_card2.is, gameo.card_list)}
          catch {case e: Exception => null}
        card2.owner_id(actioner_id).position(CardPositionEnum.HAND.toString).save
        //gameo.add_update(List(ForceUpdateEnum.USER_TABLE))
        GameProcessor.check_hand_max(gameo, actioner)
        
      case MTypeEnum.ACTION_MAGE_SHOCK =>
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString)
                 .card_enum(card.to_card.card_enum.toString).save
        val actionees = 
          try { action.action_flags2.split(",").toList.map(x => UserEntry.get(x.toLong, gameo.userentrys)) }
          catch {case e: Exception => List() }
        actionees.foreach { actionee =>
          val shock_phase = RoomPhase.createFrom(roomphase).phase_type(RoomPhaseEnum.SHOCK_REACTION.toString)
                                      .actioner_id(actionee.id.is).actionee_id(actioner.id.is).power(2)
          gameo.push_phase(shock_phase)
        }
        
      case MTypeEnum.ACTION_MAGE_SHOCK_DISCARD => 
        warn("ACTION_MAGE_SHOCK_DISCARD : " + card.toString)
        
      case MTypeEnum.ACTION_MAGE_STORM =>
        actioner.lower_gems(1).save
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString).power(2)
                 //.card_enum(card.to_card.card_enum.toString)
                 .save
        val actionees = 
          try { action.action_flags2.split(",").toList.map(x => UserEntry.get(x.toLong, gameo.userentrys)) }
          catch {case e: Exception => List() }
        actionees.foreach { actionee =>
          GameProcessor.process_check_heal(gameo, actioner, actionee)
        }
        
      case MTypeEnum.ACTION_SAINTLANCE_SHINE      =>
        //val actionee_id = action.actionee_id.is
        //val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        userentrys.foreach(_.add_heals(1).save)
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ATTACK.toString).save
        
      case MTypeEnum.ACTION_SAINTLANCE_RETRIBUTION =>
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        actionee.lower_heals(1).save
        actioner.add_heals(1).save
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ATTACK.toString).save
        
      case MTypeEnum.ACTION_SAINTLANCE_SMITE  =>
        actioner.add_heals(1).save
        GameProcessor.process_endure2(gameo, actioner, UserEntry.get(roomphase.actionee_id.is, userentrys))
        
      case MTypeEnum.ACTION_SAINTLANCE_EARTHLANCE  =>
        val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        val earthlance_power =
          PlummUtil.parseInt(action.action_flags.is)
        
        actioner.lower_heals(earthlance_power).save
        roomphase.power(roomphase.power.is + earthlance_power).save
        GameProcessor.process_endure2(gameo, actioner, UserEntry.get(roomphase.actionee_id.is, userentrys))
        
      case MTypeEnum.ACTION_SAINTLANCE_HOLYPRAY    =>  
        actioner.lower_gems(1).add_heals(2, 5).add_role_flag(UserEntryRoleFlagEnum.SAINTLANCE_NOSKYLANCE).save
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ATTACK.toString).save

      case MTypeEnum.ACTION_ELEMENTALIST_MAGIC    =>  
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        val carddata = card.to_card
        
        var power = 1
        if (carddata.cardattr_enum == CardAttrEnum.FIRE)
          power += 1
        if (cards.length != 0)
          power += 1
 
        if (carddata.cardattr_enum == CardAttrEnum.WATER) {
          val ice_target = UserEntry.get(PlummUtil.parseLong(action.action_flags2.is), userentrys)
          ice_target.add_heals(1).save
        } else if (carddata.cardattr_enum == CardAttrEnum.AIR) {
          gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ATTACK.toString).save
        } else if (carddata.cardattr_enum == CardAttrEnum.THUNDER) {
          val userentryteam = UserEntryTeam.get(actioner.team_id, gameo.userentryteams)
          userentryteam.add_gems(1).save
          gameo.add_update(List(ForceUpdateEnum.TEAM_TABLE)) 
        } else if (carddata.cardattr_enum == CardAttrEnum.EARTH) {
          gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.MAGIC.toString).save
        } 
          
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString).power(power)
                 //.card_enum(card.to_card.card_enum.toString)
                 .save
        GameProcessor.process_check_heal(gameo, actioner, actionee)
        
      case MTypeEnum.ACTION_ELEMENTALIST_IGNITE    =>  
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        actioner.lower_yellow_index(3).add_role_flag(UserEntryRoleFlagEnum.ELEMENTALIST_IGNITE).save
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.MAGIC.toString).save
        
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString).power(2)
                 //.card_enum(card.to_card.card_enum.toString)
                 .save
        GameProcessor.process_check_heal(gameo, actioner, actionee)

      case MTypeEnum.ACTION_ELEMENTALIST_MOONLIGHT    =>  
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        actioner.lower_gems(1).save
        
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString).power(1 + actioner.gems.is + actioner.crystals.is)
                 //.card_enum(card.to_card.card_enum.toString)
                 .save
        GameProcessor.process_check_heal(gameo, actioner, actionee)
        
      case MTypeEnum.ACTION_NECROMANCER_PLAGUE    =>  
        actioner.add_role_flag(UserEntryRoleFlagEnum.NECROMANCER_MAGIC).save
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString).power(1)
                 .card_enum(card.to_card.card_enum.toString)
                 .save
        val actionees = UserEntry.rrnc(actioner, gameo.userentrys) 
        actionees.foreach { actionee =>
          GameProcessor.process_check_heal(gameo, actioner, actionee)
        }
        
      case MTypeEnum.ACTION_NECROMANCER_DEATHTOUCH    =>    
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        val heals = PlummUtil.parseInt(action.action_flags2.is)
        actioner.heals(actioner.heals - heals).save // 這個不發動不朽
        val power = cards.length + heals - 3
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString).power(power)
                 //.card_enum(card.to_card.card_enum.toString)
                 .save
        GameProcessor.process_check_heal(gameo, actioner, actionee)
        
      case MTypeEnum.ACTION_NECROMANCER_GRAVEFALL     =>     
        actioner.lower_gems(1).add_role_flag(UserEntryRoleFlagEnum.NECROMANCER_MAGIC).add_heals(1).save
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString).power(2)
                 //.card_enum(card.to_card.card_enum.toString)
                 .save
        val actionees = UserEntry.rrnc(actioner, gameo.userentrys) 
        actionees.foreach { actionee =>
          GameProcessor.process_check_heal(gameo, actioner, actionee)
        }
        
      case MTypeEnum.ACTION_MAGICSWORD_GATHER =>
        //val roomphase = gameo.roomphase
        actioner.tapped(true).save
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ACTIVATE.toString).save
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString).power(1)
                 //.card_enum(card.to_card.card_enum.toString)
                 .save
        
        //gameo.add_update(List(ForceUpdateEnum.USER_TABLE))
        GameProcessor.process_check_heal(gameo, actioner, actioner)

      case MTypeEnum.ACTION_MAGICSWORD_COMET    =>    
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString).power(2)
                 //.card_enum(card.to_card.card_enum.toString)
                 .save
        GameProcessor.process_check_heal(gameo, actioner, actionee)
        
      case MTypeEnum.ACTION_JUDICATOR_BREAKRITUAL  =>
        actioner.tapped(false).fixed_hand_max(0).save
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ACTIVATE.toString).save
        val userentryteam = UserEntryTeam.get(actioner.team_id, gameo.userentryteams)
        userentryteam.add_gems(1).save
        
      case MTypeEnum.ACTION_JUDICATOR_FINALJUDGE   =>
        val power = actioner.yellow_index.is
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString).power(power)
                 //.card_enum(card.to_card.card_enum.toString)
                 .save
        actioner.yellow_index(0).save
        GameProcessor.process_check_heal(gameo, actioner, actionee)
        
      case MTypeEnum.ACTION_JUDICATOR_RITUAL       =>
        actioner.tapped(true).fixed_hand_max(5).lower_gems(1).save
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ACTIVATE.toString).save
        
        GameProcessor.check_hand_max(gameo, actioner)
        
      case MTypeEnum.ACTION_JUDICATOR_BALANCE      =>
        actioner.lower_crystals(1).add_yellow_index(1).save
        val cards_in_hand = CardPool.in_hand(actioner, gameo.card_list)
        
        if (action.action_flags.is == "1") {
          cards_in_hand.foreach(_.discard(gameo))
        } else if (action.action_flags.is == "2") {
          val card_num = actioner.get_hand_max - cards_in_hand.length
          GameProcessor.process_drawcard(gameo, actioner, card_num)
          
          val userentryteam = UserEntryTeam.get(actioner.team_id.is, gameo.userentryteams)
          if (userentryteam.gems.is + userentryteam.crystals.is < 5)
            userentryteam.gems(userentryteam.gems.is + 1).save
        }
      case MTypeEnum.ACTION_ADVENTURER_DECEIVE     =>
        if (actioner.gems.is + actioner.crystals.is < actioner.energy_max)
          actioner.crystals(actioner.crystals.is + 1).save
        val actionee = UserEntry.get(action.actionee_id, gameo.userentrys)
        val damage = 2
        val card_num_str = action.action_flags2.is.toString + "1"
        
        roomphase.phase_type(RoomPhaseEnum.ATTACK.toString).power(damage)
                 //.additional(0).additional_flag(RoomAdditionalFlagEnum.DONE.toString)
                 .last_phase_type(RoomPhaseEnum.ATTACK.toString)
                 .card_enum(card_num_str).save
                 
        GameProcessor.process_deceive_attack(gameo, action, card_num_str)
        
      case MTypeEnum.ACTION_ADVENTURER_ADDON       =>
        actioner.lower_crystals(1).add_role_flag(UserEntryRoleFlagEnum.ADVENTURER_ADDON).save
        val userentryteam = UserEntryTeam.get(actioner.team_id.is, gameo.userentryteams)
        val crystals = userentryteam.crystals.is
        userentryteam.gems(userentryteam.gems.is + crystals).crystals(0).save
        
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.BOTH.toString).save
      case MTypeEnum.ACTION_ADVENTURER_THEFT       =>
        actioner.lower_crystals(1).add_role_flag(UserEntryRoleFlagEnum.ADVENTURER_THEFT).save
        val userentryteam = UserEntryTeam.get(actioner.team_id.is, gameo.userentryteams)
        if (userentryteam.gems.is + userentryteam.crystals.is < 5)
          userentryteam.gems(userentryteam.gems.is + 1).save
        
        val enemyteam = gameo.userentryteams.filterNot(_ == userentryteam)(0)
        enemyteam.gems(enemyteam.gems.is - 1).save
        
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.BOTH.toString).save
        
      case MTypeEnum.ACTION_BISHOP_HOLYBLESS      =>
        actioner.add_heals(2).save
        
      case MTypeEnum.ACTION_BISHOP_HOLYWATER      =>
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        val card2 = CardPool.getById(action.action_card2.is, gameo.card_list)
        card2.owner_id(actionee.id.is).save
        
        actioner.add_heals(1).save
        actionee.add_heals(1).save
        
        GameProcessor.check_hand_max(gameo, actionee)
      case MTypeEnum.ACTION_BISHOP_HOLYCONTRACT   =>
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        
        val heals = PlummUtil.parseInt(action.action_flags.is)
        actioner.lower_crystals(1).heals(actioner.heals.is - heals).save
        actionee.add_heals(heals, 4).save
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ACTIVATE.toString).save
      case MTypeEnum.ACTION_BISHOP_HOLYFIELD      =>
        cards.foreach(_.discard(gameo))
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        actioner.lower_crystals(1)
        
        if (action.action_flags2.is == "2") {
          actioner.add_heals(2).save
          actionee.add_heals(1).save
        } else {
          actioner.lower_heals(1).save
          roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                   .phase_subtype(action.mtype.is.toString).power(2)
                   //.card_enum(card.to_card.card_enum.toString)
                   .save
          //gameo.add_update(List(ForceUpdateEnum.USER_TABLE))
           GameProcessor.process_check_heal(gameo, actioner, actionee)
        }
        
      case MTypeEnum.ACTION_SAGE_REFLECT           =>
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        val power = cards.length
        val process_damage_phase1 = RoomPhase.createFrom(roomphase)
                                    .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                    .actioner_id(actioner.id.is).actionee_id(actioner.id.is)
                                    .power(power) //.card_enum(card.to_card.card_enum.toString)
                                    .last_phase_type("")
        RoomPhase.push(gameo, process_damage_phase1)
        val process_damage_phase2 = RoomPhase.createFrom(roomphase)
                                    .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                    .actioner_id(actioner.id.is).actionee_id(actionee.id.is)
                                    .power(power - 1) //.card_enum(card.to_card.card_enum.toString)
                                    .last_phase_type("")
        RoomPhase.push(gameo, process_damage_phase2)
      case MTypeEnum.ACTION_SAGE_MAGICBOOK         =>
        actioner.lower_gems(1).save
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        val power = cards.length
        val process_damage_phase1 = RoomPhase.createFrom(roomphase)
                                    .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                    .actioner_id(actioner.id.is).actionee_id(actioner.id.is)
                                    .power(power - 1) //.card_enum(card.to_card.card_enum.toString)
                                    .last_phase_type("")
        RoomPhase.push(gameo, process_damage_phase1)
        val process_damage_phase2 = RoomPhase.createFrom(roomphase)
                                    .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                    .actioner_id(actioner.id.is).actionee_id(actionee.id.is)
                                    .power(power - 1) //.card_enum(card.to_card.card_enum.toString)
                                    .last_phase_type("")
        RoomPhase.push(gameo, process_damage_phase2)
      case MTypeEnum.ACTION_SAGE_HOLYBOOK          =>  
        actioner.lower_gems(1).save
        val power = cards.length
        val process_damage_phase1 = RoomPhase.createFrom(roomphase)
                                    .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                    .actioner_id(actioner.id.is).actionee_id(actioner.id.is)
                                    .power(power - 1) //.card_enum(card.to_card.card_enum.toString)
                                    .last_phase_type("")
        RoomPhase.push(gameo, process_damage_phase1)
        val actionees = 
          try { action.action_flags2.split(",").toList.map(x => UserEntry.get(x.toLong, gameo.userentrys)) }
          catch {case e: Exception => List() }        
        actionees.foreach(_.add_heals(2).save)        
        
      case MTypeEnum.ACTION_RUNEMAGE_THUNDERRUNE   =>
        val action_flags = action.action_flags.is.split(",")
        if (action.action_card2.is != 0) {
          CardPool.getByNo(action.action_card2.is.toInt, gameo.card_list)
                  .target_id(actioner.id.is)
                  .position(CardPositionEnum.BACK.toString).save
        }
        
        val is_userune  =
           ((action_flags.length > 2) && (action_flags(2) == MTypeEnum.ACTION_RUNEMAGE_USERUNE.toString))
            
        val actionees = 
          try { action.action_flags2.split(",").toList.map(x => UserEntry.get(x.toLong, userentrys)) }
          catch {case e: Exception => List() }
          
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString).power(1)

        if (is_userune) {
          actioner.lower_crystals(1).save
          roomphase.power(2)
        }
        roomphase.save
        
        actionees.foreach { actionee =>
          GameProcessor.process_check_heal(gameo, actioner, actionee)
        }
      case MTypeEnum.ACTION_RUNEMAGE_AIRRUNE       =>
        val action_flags = action.action_flags.is.split(",")
        if (action.action_card2.is != 0) {
          CardPool.getByNo(action.action_card2.is.toInt, gameo.card_list)
                  .target_id(actioner.id.is)
                  .position(CardPositionEnum.BACK.toString).save
        }
        
        val actionees = 
          (try { action.action_flags2.split(",").toList.map(x => UserEntry.get(x.toLong, userentrys)) }
          catch {case e: Exception => List() }).filter(CardPool.in_hand(_, gameo.card_list).length != 0)
          
        actionees.foreach { actionee =>
          val discard_phase1 = RoomPhase.createFrom(roomphase)
                                      .phase_type(RoomPhaseEnum.AIRRUNE_REACTION.toString)
                                      .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
                                      //.power(power) //.card_enum(card.to_card.card_enum.toString)
                                    .  last_phase_type("")
          RoomPhase.push(gameo, discard_phase1)
        }
      case MTypeEnum.ACTION_RUNEMAGE_AIRRUNE_DISCARD  =>  
        val card2 = 
          try { CardPool.getById(action.action_card2.is, gameo.card_list)}
          catch {case e: Exception => null}
        card2.discard(gameo)
        
      case MTypeEnum.ACTION_RUNEMAGE_100GHOSTS     =>
        val action_flags = action.action_flags.is.split(",")
        val is_userune  =
           ((action_flags.length > 1) && (action_flags(1) == MTypeEnum.ACTION_RUNEMAGE_USERUNE.toString))
            
        val actionees = 
          try { action.action_flags2.split(",").toList.map(x => UserEntry.get(x.toLong, userentrys)) }
          catch {case e: Exception => List() }
          
        val actionees2 =
          if (card.to_card.cardattr_enum == CardAttrEnum.FIRE)
            userentrys_rr.filterNot(actionees.contains(_))
          else actionees
          
        val power = if (is_userune)  2 else 1
        if (is_userune) 
          actioner.lower_crystals(1).save
          
        actionees2.foreach { actionee =>
          val process_damage_phase1 = RoomPhase.createFrom(roomphase)
                                      .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                      .actioner_id(actioner.id.is).actionee_id(actionee.id.is)
                                      .power(power) //.card_enum(card.to_card.card_enum.toString)
                                    .  last_phase_type("")
          RoomPhase.push(gameo, process_damage_phase1)
        }  
        GameProcessor.process_endure2(gameo, actioner, UserEntry.get(roomphase.actionee_id.is, userentrys))
        
      case MTypeEnum.ACTION_PRAYER_POWERBLESS =>
        actioner.add_role_flag(UserEntryRoleFlagEnum.PRAYER_MAGIC).save
        val actionee_id = action.actionee_id.is
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString)
                 //.additional(1).additional_flag(RoomAdditionalFlagEnum.ATTACK.toString)
                 .card_enum(card.to_card.card_enum.toString).save
        card.to_front(gameo, actioner_id, actionee_id)
        
      case MTypeEnum.ACTION_PRAYER_POWERBLESS_USE =>
        val fast_blesses = CardPool.in_front(actioner, gameo.card_list)
                                   .filter(_.to_card.has_action(MTypeEnum.ACTION_PRAYER_POWERBLESS))
        fast_blesses.foreach(_.discard(gameo))
        val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        roomphase.power(roomphase.power.is + 2).save
        GameProcessor.process_check_heal(gameo, actioner, UserEntry.get(roomphase.actionee_id.is, userentrys))

      case MTypeEnum.ACTION_PRAYER_FASTBLESS =>
        actioner.add_role_flag(UserEntryRoleFlagEnum.PRAYER_MAGIC).save
        val actionee_id = action.actionee_id.is
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString)
                 //.additional(1).additional_flag(RoomAdditionalFlagEnum.ATTACK.toString)
                 .card_enum(card.to_card.card_enum.toString).save
        card.to_front(gameo, actioner_id, actionee_id)
        
      case MTypeEnum.ACTION_PRAYER_FASTBLESS_USE =>
        //val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        val fast_blesses = CardPool.in_front(actioner, gameo.card_list)
                                   .filter(_.to_card.has_action(MTypeEnum.ACTION_PRAYER_FASTBLESS))
        fast_blesses.foreach(_.discard(gameo))
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ATTACK.toString).save

      case MTypeEnum.ACTION_PRAYER_SHINEBELIEVE =>
        cards.foreach(_.discard(gameo))
        actioner.lower_yellow_index(1)
                .add_role_flag(UserEntryRoleFlagEnum.PRAYER_MAGIC).save
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        actionee.add_heals(1).save
        val userentryteam = UserEntryTeam.get(actioner.team_id, gameo.userentryteams)
        userentryteam.add_gems(1).save

      case MTypeEnum.ACTION_PRAYER_DARKBELIEVE =>
        actioner.lower_yellow_index(1)
                .add_role_flag(UserEntryRoleFlagEnum.PRAYER_MAGIC).save
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        val process_damage_phase1 = RoomPhase.createFrom(roomphase)
                                    .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                    .actioner_id(actioner.id.is).actionee_id(actioner.id.is)
                                    .power(2) //.card_enum(card.to_card.card_enum.toString)
                                    .last_phase_type("")
        RoomPhase.push(gameo, process_damage_phase1)
        val process_damage_phase2 = RoomPhase.createFrom(roomphase)
                                    .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                    .actioner_id(actioner.id.is).actionee_id(actionee.id.is)
                                    .power(2) //.card_enum(card.to_card.card_enum.toString)
                                    .last_phase_type("")
        RoomPhase.push(gameo, process_damage_phase2)
        

      case MTypeEnum.ACTION_PRAYER_PRAY =>
        actioner.lower_gems(1).tapped(true).add_role_flag(UserEntryRoleFlagEnum.PRAYER_MAGIC).save
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ACTIVATE.toString).save
      case MTypeEnum.ACTION_PRAYER_MANATIDE =>
        actioner.lower_crystals(1).add_role_flag(UserEntryRoleFlagEnum.PRAYER_MANATIDE).save
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.MAGIC.toString).save
      case MTypeEnum.ACTION_SWORDEMP_LIBIDINALWILL =>
        actioner.lower_crystals(1).add_yellow_index(1).save
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ATTACK.toString).save
        GameProcessor.process_drawcard(gameo, actioner, 1)
        
      case MTypeEnum.ACTION_SWORDEMP_SWORDKI     =>
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        val swordki = PlummUtil.parseInt(action.action_flags.is)
        actioner.yellow_index(actioner.yellow_index.is - swordki).save
        
        val process_damage_phase1 = RoomPhase.createFrom(roomphase)
                                    .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                    .actioner_id(actioner.id.is).actionee_id(actionee.id.is)
                                    .power(swordki) //.card_enum(card.to_card.card_enum.toString)
                                    .last_phase_type("")
        RoomPhase.push(gameo, process_damage_phase1)
        GameProcessor.process_endure2(gameo, actioner, UserEntry.get(roomphase.actionee_id.is, userentrys))  

      case MTypeEnum.ACTION_MONK_KISHOT    =>
        actioner.add_yellow_index(1).save
        val actionee = UserEntry.get(action.actionee_id.is, userentrys)
        
        if (actionee.heals.is == 0) {
          val process_damage_phase1 = RoomPhase.createFrom(roomphase)
                                      .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                      .actioner_id(actioner.id.is).actionee_id(actioner.id.is)
                                      .power(actioner.yellow_index.is) //.card_enum(card.to_card.card_enum.toString)
                                      .last_phase_type("")
          RoomPhase.push(gameo, process_damage_phase1)
        }
        
        val process_damage_phase2 = RoomPhase.createFrom(roomphase)
                                    .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                    .actioner_id(actioner.id.is).actionee_id(actionee.id.is)
                                    .power(1) //.card_enum(card.to_card.card_enum.toString)
                                    .last_phase_type("")
        RoomPhase.push(gameo, process_damage_phase2)
        //GameProcessor.process_endure2(gameo, actioner, UserEntry.get(roomphase.actionee_id.is, userentrys))  
        
      case MTypeEnum.ACTION_MONK_100DRAGONS    =>
        actioner.lower_yellow_index(3).tapped(true).target_user(action.actionee_id.is).save
        
        gameo.roomround.additional(1).additional_flag("").save
      case MTypeEnum.ACTION_MONK_100DRAGONS_REMOVE    =>
        actioner.tapped(false).target_user(0).save
        gameo.roomround.additional(1).additional_flag("").save
      case MTypeEnum.ACTION_MONK_MONKGOD =>
        cards.foreach(_.discard(gameo))
        actioner.add_heals(2).lower_crystals(1).save
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ACTIVATE.toString).save
      case MTypeEnum.ACTION_BRAVE_TAUNT =>
        actioner.target_user(action.actionee_id.is).lower_yellow_index(1)
                 .add_blue_index(1).save
        //gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ACTIVATE.toString).save
      case MTypeEnum.ACTION_BRAVE_FORBIDDEN =>
        val magic_cards = cards.filter(_.to_card.cardtype_enum == CardTypeEnum.MAGIC)
        actioner.lower_crystals(1).tapped(true).fixed_hand_max(4)
                .add_yellow_index(magic_cards.length)
        GameProcessor.check_hand_max(gameo, actioner)
        if (roomphase.last_phase_type.is == RoomPhaseEnum.ATTACK.toString) {
          val fire_cards = cards.filter(_.to_card.cardattr_enum == CardAttrEnum.FIRE)
          
          if (fire_cards.length != 0) {
            roomphase.power(roomphase.power.is + fire_cards.length)
            val process_damage_phase1 = RoomPhase.createFrom(roomphase)
                                      .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                      .actioner_id(actioner.id.is).actionee_id(actioner.id.is)
                                      .power(fire_cards.length) //.card_enum(card.to_card.card_enum.toString)
                                      .last_phase_type("")
            RoomPhase.push(gameo, process_damage_phase1)
          }
            
          GameProcessor.process_endure2(gameo, actioner, UserEntry.get(roomphase.actionee_id.is, userentrys))
        } else {
          val water_cards = cards.filter(_.to_card.cardattr_enum == CardAttrEnum.WATER)
          actioner.add_blue_index(water_cards.length)
        }
        actioner.save
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ATTACK.toString).save
        
      case MTypeEnum.ACTION_BRAVE_DEATHMATCH    =>
        actioner.lower_gems(1).add_yellow_index(3).save
        //gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.BOTH.toString).save
        
      case MTypeEnum.ACTION_REDKNIGHT_BLOODPRAY    =>
        val actionees = 
          try { action.action_flags.split(",").toList.map(x => UserEntry.get(x.toLong, userentrys)) }
          catch {case e: Exception => List() }
        val heals = 
          try { action.action_flags2.split(",").toList.map(x => x.toInt) }
          catch {case e: Exception => List() }
        actioner.add_yellow_index(1).lower_heals(heals(0) + heals(1)).save
        actionees(0).add_heals(heals(0)).save
        actionees(1).add_heals(heals(1)).save
        
        val process_damage_phase1 = RoomPhase.createFrom(roomphase)
                                    .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                    .actioner_id(actioner.id.is).actionee_id(actioner.id.is)
                                    .power(heals(0) + heals(1)) //.card_enum(card.to_card.card_enum.toString)
                                    .last_phase_type("")
        RoomPhase.push(gameo, process_damage_phase1)
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ACTIVATE.toString).save
      case MTypeEnum.ACTION_REDKNIGHT_BLOODFEAST    =>
        actioner.yellow_index(actioner.yellow_index.is - 1).save
        val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
        roomphase.power(roomphase.power.is + 2).save

        GameProcessor.process_endure2(gameo, actioner, UserEntry.get(roomphase.actionee_id.is, userentrys))

        val process_damage_phase1 = RoomPhase.createFrom(roomphase)
                                    .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                    .actioner_id(actioner.id.is).actionee_id(actioner.id.is)
                                    .power(4) //.card_enum(card.to_card.card_enum.toString)
                                    .last_phase_type("")
        RoomPhase.push(gameo, process_damage_phase1)
      case MTypeEnum.ACTION_REDKNIGHT_DISCIPLINE    =>
        actioner.lower_crystals(1).tapped(false).save
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.BOTH.toString).save
      case MTypeEnum.ACTION_REDKNIGHT_BLOODCROSS    =>
        actioner.lower_crystals(1).lower_yellow_index(1).save
        val process_damage_phase1 = RoomPhase.createFrom(roomphase)
                                    .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                    .actioner_id(actioner.id.is).actionee_id(action.actionee_id.is)
                                    .power(3) //.card_enum(card.to_card.card_enum.toString)
                                    .last_phase_type("")
        RoomPhase.push(gameo, process_damage_phase1)
        val process_damage_phase2 = RoomPhase.createFrom(roomphase)
                                    .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                    .actioner_id(actioner.id.is).actionee_id(actioner.id.is)
                                    .power(4) //.card_enum(card.to_card.card_enum.toString)
                                    .last_phase_type("")
        RoomPhase.push(gameo, process_damage_phase2)
        
      case MTypeEnum.ACTION_SOULMAGE_SOULGIVE       =>
        actioner.lower_blue_index(3).save
        val actionee = UserEntry.get(action.actionee_id.is, userentrys)
        actionee.add_gems(2).save
        //simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用靈魂賜予" , true, "hunter-do")
      case MTypeEnum.ACTION_SOULMAGE_SOULBURST      =>
        actioner.lower_yellow_index(3).save
        val actionee = UserEntry.get(action.actionee_id.is, userentrys)
        val actionee_hands = CardPool.in_hand(actionee, gameo.card_list)
        val power = 
          if ((actionee_hands.length < 3) && (actionee.get_hand_max > 5)) 5
          else 3
          
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString).power(power)
                 //.card_enum(card.to_card.card_enum.toString)
                 .save
        GameProcessor.process_check_heal(gameo, actioner, actionee)
      case MTypeEnum.ACTION_SOULMAGE_SOULSUMMON     =>
        actioner.add_blue_index(cards.length + 1).save
      case MTypeEnum.ACTION_SOULMAGE_SOULMIRROR     =>
        cards.foreach(_.discard(gameo))
        actioner.lower_yellow_index(2).save
        val actionee = UserEntry.get(action.actionee_id.is, userentrys)
        val draw_num = math.min(3, actionee.get_hand_max - CardPool.in_hand(actionee, gameo.card_list).length)
        GameProcessor.process_drawcard(gameo, actionee, draw_num)
      case MTypeEnum.ACTION_SOULMAGE_SOULLINK          =>
        actioner.lower_yellow_index(1).lower_blue_index(1)
                .target_user(action.actionee_id.is).save
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ACTIVATE.toString).save
      case MTypeEnum.ACTION_SOULMAGE_SOULLINK_TRANSFER =>
        val heals     = action.action_flags.is.toInt
        actioner.lower_blue_index(heals).save
        
        val actionee    = UserEntry.get(roomphase.actionee_id.is, userentrys)
        val target_user = UserEntry.get(actioner.target_user.is, userentrys)
        
        if (roomphase.phase_flags.is == SoulMageEnum.TOMAGE.toString) {
          val power = roomphase.power.is
          val last_phase_type = roomphase.last_phase_type.is
          roomphase.power(heals).last_phase_type("")
          GameProcessor.process_damage(gameo, actionee, actioner)
          roomphase.power(power - heals).last_phase_type(last_phase_type)
          GameProcessor.process_damage(gameo, actionee, target_user)
        } else if (roomphase.phase_flags.is == SoulMageEnum.FROMMAGE.toString) {
          val power = roomphase.power.is
          val last_phase_type = roomphase.last_phase_type.is
          roomphase.power(heals).last_phase_type("")
          GameProcessor.process_damage(gameo, actionee, target_user)
          roomphase.power(power - heals).last_phase_type(last_phase_type)
          GameProcessor.process_damage(gameo, actionee, actioner)
        } else
          warn("Unknown SoulLink Transfer : " + roomphase.phase_flags.is)
      case MTypeEnum.ACTION_SOULMAGE_SOULENHANCE     =>
        actioner.lower_gems(1).add_yellow_index(2).add_blue_index(2).save
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ACTIVATE.toString).save
        
      case MTypeEnum.ACTION_MIKO_BLOODCRY          =>
        val power = PlummUtil.parseInt(action.action_flags2.is)
        val actionee    = UserEntry.get(action.actionee_id.is, userentrys)
        val process_damage_phase1 = RoomPhase.createFrom(roomphase)
                                    .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                    .actioner_id(actioner.id.is).actionee_id(actioner.id.is)
                                    .power(power) //.card_enum(card.to_card.card_enum.toString)
                                    .last_phase_type("")
        RoomPhase.push(gameo, process_damage_phase1)
        val process_damage_phase2 = RoomPhase.createFrom(roomphase)
                                    .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                    .actioner_id(actioner.id.is).actionee_id(actionee.id.is)
                                    .power(power) //.card_enum(card.to_card.card_enum.toString)
                                    .last_phase_type("")
        RoomPhase.push(gameo, process_damage_phase2)
      case MTypeEnum.ACTION_MIKO_STICKWITH         =>
        val old_target_user_id = actioner.target_user.is
        val actionee    = UserEntry.get(action.actionee_id.is, userentrys)
        actioner.target_user(actionee.id.is)
        if (actioner.tapped.is) {
          actionee.hand_max(actionee.hand_max.is + 1).save
          if (old_target_user_id != 0) {
            val old_target_user = UserEntry.get(old_target_user_id, userentrys)
            old_target_user.hand_max(old_target_user.hand_max.is - 1).save
            GameProcessor.check_hand_max(gameo, old_target_user)
          } else
            actioner.hand_max(actioner.hand_max.is + 1).save
        } else {
          actionee.hand_max(actionee.hand_max.is - 2).save
          GameProcessor.check_hand_max(gameo, actionee)
          if (old_target_user_id != 0) {
            val old_target_user = UserEntry.get(old_target_user_id, userentrys)
            old_target_user.hand_max(old_target_user.hand_max.is + 2).save
          } else
            actioner.hand_max(actioner.hand_max.is - 2).save
        }
        GameProcessor.process_drawcard(gameo, actioner, 2)
        
      case MTypeEnum.ACTION_MIKO_BLOODSORROW       =>
        val old_target_user = UserEntry.get(actioner.target_user.is, userentrys)
        if (action.action_flags.is == "1") {
          actioner.target_user(0)
          if (actioner.tapped.is) {
            actioner.hand_max(actioner.hand_max.is - 1).save
            GameProcessor.check_hand_max(gameo, actioner)
            old_target_user.hand_max(old_target_user.hand_max.is - 1).save
            GameProcessor.check_hand_max(gameo, old_target_user)
          } else {
            actioner.hand_max(actioner.hand_max.is + 2).save
            old_target_user.hand_max(old_target_user.hand_max.is + 2).save
          }
        } else {
          val actionee    = UserEntry.get(action.actionee_id.is, userentrys)
          actioner.target_user(actionee.id.is).save
          if (actioner.tapped.is) {
            actionee.hand_max(actionee.hand_max.is + 1).save
            old_target_user.hand_max(old_target_user.hand_max.is - 1).save
            GameProcessor.check_hand_max(gameo, old_target_user)
          } else {
            actionee.hand_max(actionee.hand_max.is - 2).save
            old_target_user.hand_max(old_target_user.hand_max.is + 2).save
            GameProcessor.check_hand_max(gameo, actionee)
          }
        }
        val process_damage_phase = RoomPhase.createFrom(roomphase)
                                   .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                   .actioner_id(actioner.id.is).actionee_id(actioner.id.is)
                                   .power(2) //.card_enum(card.to_card.card_enum.toString)
                                   .last_phase_type("")
        RoomPhase.push(gameo, process_damage_phase)
        gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ACTIVATE.toString).save
      case MTypeEnum.ACTION_MIKO_REVERSEBLEED      =>
        cards.foreach(_.discard(gameo))
        actioner.add_heals(1).save
      case MTypeEnum.ACTION_MIKO_BLOODCURSE        =>
        cards.foreach(_.discard(gameo))
        actioner.lower_gems(1).save
        val actionee    = UserEntry.get(action.actionee_id.is, userentrys)
        roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                 .phase_subtype(action.mtype.is.toString).power(2)
                 .save
        GameProcessor.process_check_heal(gameo, actioner, actionee)
      case MTypeEnum.ACTION_BUTTERFLY_DANCE        =>  
        if (action.action_flags.is == "") {
          GameProcessor.process_drawcard(gameo, actioner, 1)
        }
        val card1 = CardHelper.draw_card(gameo)
        card1.target_id(actioner.id.is).position(CardPositionEnum.BACK.toString).save
        gameo.room.save
        if (CardPool.in_back(actioner, gameo.card_list).length > RoleButterfly.role_back_cards_max) {
          val backdiscard_phase = RoomPhase.createFrom(roomphase)
                                   .phase_type(RoomPhaseEnum.BACKDISCARD_REACTION.toString)
                                   .actioner_id(actioner.id.is)
                                   .last_phase_type("")
          RoomPhase.push(gameo, backdiscard_phase)
        }
      case MTypeEnum.ACTION_BUTTERFLY_PILGRIMAGE     =>  
        val roomphase = gameo.roomphase
        val actionee = UserEntry.get(roomphase.actionee_id, gameo.userentrys)
        roomphase.power(roomphase.power.is - 1).save
        if (action.action_flags2.is != "0") {
          //val damage_actionee = UserEntry.get(PlummUtil.parseLong(action.action_flags2.is), gameo.userentrys)
          val process_damage_phase1 = RoomPhase.createFrom(roomphase)
                                     .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                     .actioner_id(actioner.id.is).actionee_id(actioner.id.is)
                                     .power(2) //.card_enum(card.to_card.card_enum.toString)
                                     .last_phase_type("")
          gameo.push_phase(process_damage_phase1)
          val process_damage_phase2 = RoomPhase.createFrom(roomphase)
                                     .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                     .actioner_id(actioner.id.is).actionee_id(PlummUtil.parseLong(action.action_flags2.is))
                                     .power(1) //.card_enum(card.to_card.card_enum.toString)
                                     .last_phase_type("")
          gameo.push_phase(process_damage_phase2)
          val enemyteam = gameo.userentryteams.filter(_.id.is != actioner.team_id.is)(0)
          if (enemyteam.hasnt_team_flag(UserEntryTeamFlagEnum.MORALGUARD))
            enemyteam.add_team_flag(UserEntryTeamFlagEnum.MORALGUARD).save
        }
        GameProcessor.process_damage(gameo, actionee, actioner)

      case MTypeEnum.ACTION_BUTTERFLY_POISONPOWDER   =>  
        val roomphase = gameo.roomphase
        val actionee  = UserEntry.get(roomphase.actionee_id, gameo.userentrys)
        val actionee2 = UserEntry.get(roomphase.actionee2_id, gameo.userentrys)
        roomphase.power(roomphase.power.is + 1).save
        if (action.action_flags2.is != "0") {
          //val damage_actionee = UserEntry.get(PlummUtil.parseLong(action.action_flags2.is), gameo.userentrys)
          val process_damage_phase1 = RoomPhase.createFrom(roomphase)
                                     .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                     .actioner_id(actioner.id.is).actionee_id(actioner.id.is)
                                     .power(2) //.card_enum(card.to_card.card_enum.toString)
                                     .last_phase_type("")
          gameo.push_phase(process_damage_phase1)
          val process_damage_phase2 = RoomPhase.createFrom(roomphase)
                                     .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                     .actioner_id(actioner.id.is).actionee_id(PlummUtil.parseLong(action.action_flags2.is))
                                     .power(1) //.card_enum(card.to_card.card_enum.toString)
                                     .last_phase_type("")
          gameo.push_phase(process_damage_phase2)
          val enemyteam = gameo.userentryteams.filter(_.id.is != actioner.team_id.is)(0)
          if (enemyteam.hasnt_team_flag(UserEntryTeamFlagEnum.MORALGUARD))
            enemyteam.add_team_flag(UserEntryTeamFlagEnum.MORALGUARD).save
        }
        GameProcessor.process_post_heal(gameo, actionee, actionee2)
      case MTypeEnum.ACTION_BUTTERFLY_MIRRORFLOWER   =>  
        val roomphase = gameo.roomphase
        val actionee  = UserEntry.get(roomphase.actionee_id, gameo.userentrys)
        val actionee2 = UserEntry.get(roomphase.actionee2_id, gameo.userentrys)
        roomphase.power(roomphase.power.is + 1).save
        for (i <- 0 until 2) {
          val process_damage_phase = RoomPhase.createFrom(roomphase)
                                     .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                     .actioner_id(actioner.id.is).actionee_id(actionee2.id.is)
                                     .power(1) //.card_enum(card.to_card.card_enum.toString)
                                     .last_phase_type("")
          gameo.push_phase(process_damage_phase)
        }
        val action_flags2 = action.action_flags2.split(",")
        action_flags2.foreach { action_flags_1 =>
          if (action_flags_1 != "0") {
            //val damage_actionee = UserEntry.get(PlummUtil.parseLong(action.action_flags2.is), gameo.userentrys)
            val process_damage_phase1 = RoomPhase.createFrom(roomphase)
                                       .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                       .actioner_id(actioner.id.is).actionee_id(actioner.id.is)
                                       .power(2) //.card_enum(card.to_card.card_enum.toString)
                                       .last_phase_type("")
            gameo.push_phase(process_damage_phase1)
            val process_damage_phase2 = RoomPhase.createFrom(roomphase)
                                       .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                       .actioner_id(actioner.id.is).actionee_id(PlummUtil.parseLong(action_flags_1))
                                       .power(1) //.card_enum(card.to_card.card_enum.toString)
                                       .last_phase_type("")
            gameo.push_phase(process_damage_phase2)
            val enemyteam = gameo.userentryteams.filter(_.id.is != actioner.team_id.is)(0)
            if (enemyteam.hasnt_team_flag(UserEntryTeamFlagEnum.MORALGUARD))
              enemyteam.add_team_flag(UserEntryTeamFlagEnum.MORALGUARD).save
            }
        }
      case MTypeEnum.ACTION_BUTTERFLY_COCOON        =>  
        actioner.add_yellow_index(1).lower_gems(1).save
        for (i <- 0 until 4) {
          val card = CardHelper.draw_card(gameo)
          card.target_id(actioner.id.is).position(CardPositionEnum.BACK.toString).save
        }
        gameo.room.save
        if (CardPool.in_back(actioner, gameo.card_list).length > RoleButterfly.role_back_cards_max) {
          val backdiscard_phase = RoomPhase.createFrom(roomphase)
                                   .phase_type(RoomPhaseEnum.BACKDISCARD_REACTION.toString)
                                   .actioner_id(actioner.id.is)
                                   .last_phase_type("")
          RoomPhase.push(gameo, backdiscard_phase)
        }
        GameProcessor.check_hand_max(gameo, actioner)
      case MTypeEnum.ACTION_BUTTERFLY_REVERSEFLY        =>  
        actioner.lower_crystals(1).save
        val message_flags1 = action.action_flags.is.split(",")
        val message_flags2 = action.action_flags2.is.split(",")
        if (action.actionee_id.is != 0) {
          val actionee    = UserEntry.get(action.actionee_id.is, userentrys)
          roomphase.phase_type(RoomPhaseEnum.MAGIC.toString)
                   .phase_subtype(action.mtype.is.toString).power(1)
                   .save
          GameProcessor.process_post_heal(gameo, actioner, actionee)
        } else if (message_flags1.length == 4) {
          actioner.lower_yellow_index(1).save
          message_flags2.foreach { action_flags_1 =>
            if (action_flags_1 != "0") {
              //val damage_actionee = UserEntry.get(PlummUtil.parseLong(action.action_flags2.is), gameo.userentrys)
              val process_damage_phase1 = RoomPhase.createFrom(roomphase)
                                         .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                         .actioner_id(actioner.id.is).actionee_id(actioner.id.is)
                                         .power(2) //.card_enum(card.to_card.card_enum.toString)
                                         .last_phase_type("")
              gameo.push_phase(process_damage_phase1)
              val process_damage_phase2 = RoomPhase.createFrom(roomphase)
                                         .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                         .actioner_id(actioner.id.is).actionee_id(PlummUtil.parseLong(action_flags_1))
                                         .power(1) //.card_enum(card.to_card.card_enum.toString)
                                         .last_phase_type("")
              gameo.push_phase(process_damage_phase2)
              val enemyteam = gameo.userentryteams.filter(_.id.is != actioner.team_id.is)(0)
              if (enemyteam.hasnt_team_flag(UserEntryTeamFlagEnum.MORALGUARD))
                enemyteam.add_team_flag(UserEntryTeamFlagEnum.MORALGUARD).save
              }
          }
        } else {
          actioner.lower_yellow_index(1).save
          val process_damage_phase = RoomPhase.createFrom(roomphase)
                                              .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                              .actioner_id(actioner.id.is).actionee_id(actioner.id.is)
                                              .power(4) //.card_enum(card.to_card.card_enum.toString)
                                              .last_phase_type("")
          gameo.push_phase(process_damage_phase)
        }
        
      case xs =>
        warn("Unprocessed Action : " + action.toString)

    }
    
    //GameProcessor.next_phase(gameo) //, action)
  }
}
