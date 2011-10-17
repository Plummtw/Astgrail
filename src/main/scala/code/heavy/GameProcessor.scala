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

//import scala.collection.JavaConversions
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer 

import org.plummtw.astgrail.model._
import org.plummtw.astgrail.enum._
import org.plummtw.astgrail.actor._
import org.plummtw.astgrail.data._
import org.plummtw.astgrail.card._
import org.plummtw.astgrail.snippet.UserEntrySnippet
import org.plummtw.astgrail.util.PlummUtil
import org.plummtw.astgrail.util.CardHelper

object GameProcessor extends Logger{
  val random = scala.util.Random
  
  // 分配職業
  def dispatch_role(room : Room, userentrys_rr : List[UserEntry], userentryteams : List[UserEntryTeam]) {
    var all_role_list = List(
      RoleEnum.ARCHER, RoleEnum.ASSASSIN, RoleEnum.SWORDSAINT, RoleEnum.ANGEL, 
      RoleEnum.SAINTGIRL, RoleEnum.BERSERKER, RoleEnum.SEALER, RoleEnum.MAGE)
    
    if (room.max_stars.is >= 7)
      all_role_list = all_role_list ::: 
        List(RoleEnum.SAINTLANCE, RoleEnum.ELEMENTALIST, RoleEnum.NECROMANCER, RoleEnum.MAGICSWORD,
             RoleEnum.JUDICATOR,  RoleEnum.ADVENTURER)
        
    if (room.max_stars.is >= 8)
      all_role_list = all_role_list ::: 
        List(RoleEnum.BISHOP, RoleEnum.SAGE, RoleEnum.RUNEMAGE, RoleEnum.PRAYER, RoleEnum.REDKNIGHT)

    if (room.max_stars.is >= 9)
      all_role_list = all_role_list ::: 
        List(RoleEnum.SWORDEMP, RoleEnum.MONK, RoleEnum.BRAVE, RoleEnum.SOULMAGE)

    if (room.max_stars.is >= 10)
      all_role_list = all_role_list ::: 
        List(RoleEnum.MIKO, RoleEnum.BUTTERFLY)

    
    var java_role_list : java.util.LinkedList[String] = new java.util.LinkedList()
    all_role_list.foreach { role =>
      java_role_list.add(role.toString)
    }
    
    java.util.Collections.shuffle(java_role_list)
    
    // 設定玩家優先順位
    var java_user_no_array : java.util.LinkedList[Int] = new java.util.LinkedList()
    for (i <- 1 to userentrys_rr.length)
      java_user_no_array.add(i)
      
    java.util.Collections.shuffle(java_user_no_array)
    
    userentrys_rr.foreach { userentry =>
      userentry.user_no(java_user_no_array.removeFirst()).room_flags(userentry.role.is).role("")
      //println(user_entry.user_no.is + " " + user_entry.user_flags.is)
    }

    val userentrys_ordered = userentrys_rr.sortBy(_.user_no.is)

    // 第一次先看看有沒有希望陣營
    userentrys_ordered.foreach { userentry =>
      if ((random.nextInt(3) != 0)) {
        //if (userentry.role_flags.is == java_role_list.get(0).toString) {
        //  userentry.role(java_role_list.removeFirst().toString)
        //}
        val role_index = java_role_list.indexOf(userentry.role_flags.is)
        if (role_index != -1) {
          userentry.role(userentry.role_flags.is)
          java_role_list.remove(userentry.role_flags.is)
        }
      }
    }

    // 然後設定剩下的職業
    def first_with_level(java_role_list1 : java.util.LinkedList[String], level : Int) : Int = {
      for (i <- 0 until java_role_list1.length) {
        val role_level = RoleEnum.get_role(java_role_list1.get(i)).role_level
        if (role_level <= level)
          return i
      }
      0
    }
    
    userentrys_ordered.foreach { userentry =>
      if (userentry.role.is == "") {
        var player_level = PlummUtil.parseInt(userentry.user_flags)
        if ((player_level < 6) || (player_level > 10))
          player_level = 6
        
        val role_index = first_with_level(java_role_list, player_level)
        val role_got   = java_role_list(role_index)
        userentry.role(role_got)
        java_role_list.remove(role_got)
        //userentry.role(java_role_list.removeFirst())
      }
    }
    
    // 亂數隊伍
    var java_team_list : java.util.LinkedList[UserEntryTeam] = new java.util.LinkedList()
    for ( i <- 0 until userentrys_rr.length )
      java_team_list.add(userentryteams(i % userentryteams.length))
    
    java.util.Collections.shuffle(java_team_list)
    
    userentrys_ordered.foreach { userentry =>
      val index = java_team_list.indexWhere(_.team_type.is.toString == userentry.room_flags.is)
      if (index != -1) {
        val team = java_team_list(index)
        userentry.team_id(team.id.is)
        java_team_list.remove(index)
      } else
        userentry.team_id(0)
    }
    
    userentrys_ordered.foreach { userentry =>
      if (userentry.team_id.is == 0) {
        userentry.team_id(java_team_list.removeFirst())
      }
    }
    
    
    //java_user_no_array : java.util.LinkedList[Int] = new java.util.LinkedList()
    for (i <- 1 to userentrys_rr.length)
      java_user_no_array.add(i)
    
    if (room.has_flag(RoomFlagEnum.RANDOM_POSITION))
      java.util.Collections.shuffle(java_user_no_array)
    
    userentrys_rr.foreach { userentry =>
      val team_index = userentry.user_no.is - 1
      userentry.team_id(userentryteams(team_index % userentryteams.length).id.is)
      
      userentry.user_no(java_user_no_array.removeFirst()).room_flags("").role_flags("").user_flags("")
      
      if ((userentry.get_role == RoleJudicator) || (userentry.get_role == RoleBrave))
        userentry.crystals(2)
      //println(user_entry.user_no.is + " " + user_entry.user_flags.is)
      userentry.save
    }
  }
  
  def process_start_game(gameo : GameObject) = {
    info("process_start_game")
    val room = gameo.room
    var userentrys_rr = UserEntry.findAll(By(UserEntry.room_id, room.id.is),
                                          By(UserEntry.revoked, false))
    // 建立紅藍二隊
    val red_team  = UserEntryTeam.create.room_id(room.id.is).team_type(UserEntryTeamTypeEnum.RED.toString)
    if (userentrys_rr.length > 6)
      red_team.moral(18)
    red_team.save
    val blue_team = UserEntryTeam.create.room_id(room.id.is).team_type(UserEntryTeamTypeEnum.BLUE.toString)
    if (userentrys_rr.length > 6)
      blue_team.moral(18)
    blue_team.save
    
    dispatch_role(room, userentrys_rr, List(red_team, blue_team))
    
    if (room.has_flag(RoomFlagEnum.RANDOM_POSITION))
      userentrys_rr = UserEntry.findAll(By(UserEntry.room_id, room.id.is),
                                      By(UserEntry.revoked, false),
                                      OrderBy(UserEntry.user_no, Ascending))

    // 洗牌 移至建村時洗牌
    //CardHelper.shuffle_cardpool(room, List())
    
    // 每人抽四張初期手牌
    val card_list = CardPool.findAllByRoomId(room.id.is)
    userentrys_rr.foreach { userentry =>
      for (i <- 0 until 4) {
        val cardpool = CardHelper.draw_card(gameo)
        cardpool.owner_id(userentry.id.is).position(CardPositionEnum.HAND.toString).save
      }
    }
    room.stack_index(-1)
    room.save
    
    
    // 加入第一回合
    val new_round = RoomRound.create.room_id(room.id.is).round_no(1).turn_no(1).actioner_id(userentrys_rr(0).id.is)
    new_round.save
    
    gameo.roomround  = new_round
    gameo.roomphases = List()
    
    val new_phase = RoomPhase.create.roomround_id(new_round.id.is)
                    .phase_type(RoomPhaseEnum.MAIN.toString)
                    .actioner_id(userentrys_rr(0).id.is)
                    .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
    RoomPhase.push(gameo, new_phase)
    //new_phase.save

    val talk = Talk.create.roomround_id(new_round.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                          .message("第 " + (new_round.round_no.is.toString) + " 日 "+ (new java.util.Date).toString)
    talk.save
    
    room.status(RoomStatusEnum.PLAYING.toString)
    room.save
  }
  
  def process_room(gameo: GameObject, action: Action) : Unit = {
    info("process room : " + action.mtype.is.toString)
    val room = gameo.room
    val actioner_id   = action.actioner_id.is
    val actioner      = UserEntry.get(actioner_id, gameo.userentrys)
    val action_enum   = MTypeEnum.get_action(action.mtype.is)
    val userentrys    = gameo.userentrys
    val userentrys_rr = UserEntry.rr(userentrys)
    
    action_enum match {
      case MTypeEnum.ACTION_KICK =>
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        actionee.add_room_flag(UserEntryRoomFlagEnum.KICKED)
        actionee.save

        // 如果被踢到 3 次
        //val room = Room.find(By(Room.id, actionee.room_id.is)).get

        if (actionee.room_flags.is.count(_.toString == UserEntryRoomFlagEnum.KICKED.toString) >= 3) {
          UserEntrySnippet.revoke(room, actionee)
          RoomActor.sendUserEntryMessage(actionee.id.is, ForceOut(actionee.id.is))
        }
        
        val userentrys_reload = UserEntry.findAllByRoom(room)
        RoomActor.sendRoomMessage(actionee.room_id.is, SessionVarSet(room = room, userentrys = userentrys_reload))
        RoomActor.sendRoomMessage(actionee.room_id.is, RoomForceUpdate(actionee.room_id.is ,List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.ACTION_BAR)))
        
        return

      case MTypeEnum.ACTION_STARTGAME =>
        actioner.add_room_flag(UserEntryRoomFlagEnum.VOTED)
        actioner.save

        // 如果全員都開始遊戲
        val userentrys_notready = userentrys_rr.filter(x => (x.hasnt_room_flag(UserEntryRoomFlagEnum.VOTED)))

        if ((userentrys_rr.length >= 4) && (userentrys_rr.length % 2 == 0) &&
            (userentrys_notready.length == 0)) {
          
          room.status(RoomStatusEnum.PLAYING.toString)
          room.save
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room))
          
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is,
                                                                         List(ForceUpdateEnum.GO_OUT_LINK)))
  
          GameProcessor.process_start_game(gameo)
          // New Round
          //val room_reload       = Room.find(By(Room.id, actioner.room_id.is)).get
          val roomround_reload  = RoomRound.find(By(RoomRound.room_id, room.id.is),
                                                 OrderBy(RoomRound.round_no, Descending)).get
          val roomphases_reload = RoomPhase.findAllByRoomRoundId(roomround_reload.id.is)
          val roomphase_reload  = RoomPhase.get_phase(room, roomphases_reload)
          val userentrys_reload = UserEntry.findAllByRoom(room)
          val userentryteams = UserEntryTeam.findAll(By(UserEntryTeam.room_id, room.id.is))
          val card_list  = CardPool.findAllByRoomId(room.id.is)

          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomround = roomround_reload,
                                                                       roomphases = roomphases_reload,
                                                                       userentrys = userentrys_reload,
                                                                       userentryteams = userentryteams,
                                                                       card_list = card_list))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is,
                                                                         List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.TEAM_TABLE,
                                                                              ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.ACTION_BAR, ForceUpdateEnum.CARD_TABLE)))
        } else {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }
    }
  }  
  
  def process_main(gameo: GameObject, action: Action) : Unit = {
    if (action != null)
      info("process main : " + action.mtype.is.toString)
    
    //gameo.room.processing(true).save
    var peeked_phase : RoomPhase = null
    
    do {
      if (peeked_phase == null)
        info("do : null")
      else
        info("do : " + peeked_phase.toString)
     
      
      gameo.room.processing(true).save
      gameo.set_delayed_action(false)
      if (!gameo.is_next_turn)
      {
        val poped_phase = gameo.pop_phase()
        poped_phase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
        if (poped_phase.phase_type.is == RoomPhaseEnum.DELAYED_ACTION.toString) {
          poped_phase.save
          val delayed_action = Action.find(By(Action.id, poped_phase.phase_flags.is.toString.toLong)).get
          //info("Delayed Action : "  + gameo.room.toString)
          //info("Delayed Action :  " + gameo.roomphases.toString)
          //gameo.set_is_first(false)
          //gameo.set_delayed_action(true)
          gameo.set_delayed_action(true)
          ActionHelper.process_action(gameo, delayed_action)
        } else if (poped_phase.phase_type.is == RoomPhaseEnum.PROCESS_DAMAGE.toString) {
          poped_phase.save
          val actioner = UserEntry.get(poped_phase.actioner_id.is, gameo.userentrys)
          val actionee = UserEntry.get(poped_phase.actionee_id.is, gameo.userentrys)
          process_check_heal(gameo, actioner, actionee)
        } else if (poped_phase.phase_type.is == RoomPhaseEnum.PROCESS_POISON.toString) {
          poped_phase.save
          
          val card = CardPool.getById(poped_phase.action_card.is, gameo.card_list)
          if (card.position.is == CardPositionEnum.FRONT.toString) {
            card.discard(gameo)
            val actioner = UserEntry.get(poped_phase.actioner_id.is, gameo.userentrys)
            val actionee = UserEntry.get(poped_phase.actionee_id.is, gameo.userentrys)
            process_check_heal(gameo, actioner, actionee)
          }
        } else {
          val actioner = UserEntry.get(poped_phase.actioner_id.is, gameo.userentrys)
          if (actioner.has_room_flag(UserEntryRoomFlagEnum.SKIPPED))
            poped_phase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), 5))
          poped_phase.save
          ActionHelper.process_action(gameo, action)
        }
        
        if (check_victory(gameo)) {
          process_victory(gameo)
          return
        }
        
        peeked_phase = gameo.peek_phase
        
        // 自動跳過不符合的 DISCARD PHASE
        if (peeked_phase != null) {
          if (peeked_phase.phase_type.is == RoomPhaseEnum.DISCARD_REACTION.toString) {
            val userentry = UserEntry.get(peeked_phase.actioner_id.is, gameo.userentrys)
            if (CardPool.in_hand(userentry, gameo.card_list).length <= userentry.get_hand_max) {
              // 跳過
              gameo.pop_phase()
              peeked_phase = gameo.peek_phase
            }
          } else if (peeked_phase.phase_type.is == RoomPhaseEnum.WEAKEN_REACTION.toString) {
            val card = CardPool.getById(peeked_phase.action_card.is, gameo.card_list)
            if (card.position.is != CardPositionEnum.FRONT.toString) {
              // 跳過
              gameo.pop_phase()
              peeked_phase = gameo.peek_phase
            }
          } /* else if (peeked_phase.phase_type.is == RoomPhaseEnum.HEAL_REACTION.toString) {
            val userentry = UserEntry.get(peeked_phase.actioner_id.is, gameo.userentrys)
            if (userentry.heals.is == 0) {
              // 跳過
              gameo.pop_phase()
              peeked_phase = gameo.peek_phase
            }
          } */
        }

        if (peeked_phase == null) {
          val round_player = UserEntry.get(gameo.roomround.actioner_id.is, gameo.userentrys)
          //val role = current_player.get_role
          if ((round_player.get_role == RoleSwordSaint) && (round_player.has_role_flag(UserEntryRoleFlagEnum.ATTACK)) && 
              (gameo.roomround.additional.is == 0) &&
              (round_player.hasnt_role_flag(UserEntryRoleFlagEnum.SWORDSAINT_DONE))) {
            warn("RoleSwordSaint Additional Turn Reaction")
            val action_phase = RoomPhase.create.roomround_id(gameo.roomround.id.is)
                                        .phase_type(RoomPhaseEnum.ADDITIONALTURN_REACTION.toString).actioner_id(round_player.id.is)
                                        .deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
                               //.phase_flags(roomphase.phase_type.is)
            gameo.push_phase(action_phase)
            peeked_phase  = action_phase
          } else if ((round_player.get_role == RoleMagicSword) && (round_player.has_role_flag(UserEntryRoleFlagEnum.ATTACK)) && 
              (gameo.roomround.additional.is == 0) &&
              (round_player.hasnt_role_flag(UserEntryRoleFlagEnum.MAGICSWORD_COMBO))) {
            round_player.add_role_flag(UserEntryRoleFlagEnum.MAGICSWORD_COMBO).save
            gameo.roomround.additional(1).additional_flag(RoomAdditionalFlagEnum.ATTACK_FIRE.toString).save
          } else if ((round_player.get_role == RolePrayer) && (round_player.has_role_flag(UserEntryRoleFlagEnum.PRAYER_MAGIC)) && 
              (gameo.roomround.additional.is == 0) &&
              (round_player.hasnt_role_flag(UserEntryRoleFlagEnum.PRAYER_MANATIDE) &&
              (round_player.gems.is + round_player.crystals.is > 0))) {
            val action_phase = RoomPhase.create.roomround_id(gameo.roomround.id.is)
                                        .phase_type(RoomPhaseEnum.MANATIDE_REACTION.toString).actioner_id(round_player.id.is)
                                        .deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
                               //.phase_flags(roomphase.phase_type.is)
            gameo.push_phase(action_phase)
            peeked_phase  = action_phase
          } else if ((round_player.get_role == RoleSwordEmp) &&
              (gameo.roomround.additional.is == 0) &&
              (round_player.hasnt_role_flag(UserEntryRoleFlagEnum.SWORDEMP_LIBIDINALWILL_DONE)) &&
              (round_player.has_role_flag(UserEntryRoleFlagEnum.ATTACK)) &&
              (round_player.gems.is + round_player.crystals.is > 0)) {
            val action_phase = RoomPhase.create.roomround_id(gameo.roomround.id.is)
                                        .phase_type(RoomPhaseEnum.LIBIDINALWILL_REACTION.toString).actioner_id(round_player.id.is)
                                        .deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
                               //.phase_flags(roomphase.phase_type.is)
            gameo.push_phase(action_phase)
            peeked_phase  = action_phase
          } else if ((round_player.get_role == RoleRedKnight) && (round_player.tapped.is) &&
              (gameo.roomround.additional.is == 0) &&
              (round_player.hasnt_role_flag(UserEntryRoleFlagEnum.REDKNIGHT_DISCIPLINE)) &&
              ((round_player.has_role_flag(UserEntryRoleFlagEnum.ATTACK)) ||
               (round_player.has_role_flag(UserEntryRoleFlagEnum.MAGIC))) &&
              (round_player.gems.is + round_player.crystals.is > 0)) {
            val action_phase = RoomPhase.create.roomround_id(gameo.roomround.id.is)
                                        .phase_type(RoomPhaseEnum.DISCIPLINE_REACTION.toString).actioner_id(round_player.id.is)
                                        .deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
                               //.phase_flags(roomphase.phase_type.is)
            gameo.push_phase(action_phase)
            peeked_phase  = action_phase
          } else if ((gameo.roomround.additional.is == 0) &&
                     (round_player.hasnt_role_flag(UserEntryRoleFlagEnum.SPECIAL)) &&
                     (round_player.hasnt_user_flag(UserEntryFlagEnum.FASTBLESSED)) &&
                     (CardPool.in_front(round_player, gameo.card_list)
                              .filter(_.to_card.has_action(MTypeEnum.ACTION_PRAYER_FASTBLESS)).length != 0)) {
            val action_phase = RoomPhase.create.roomround_id(gameo.roomround.id.is)
                                        .phase_type(RoomPhaseEnum.FASTBLESS_REACTION.toString).actioner_id(round_player.id.is)
                                        .deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
                               //.phase_flags(roomphase.phase_type.is)
            gameo.push_phase(action_phase)
            peeked_phase  = action_phase
           }
        }
      
        if (peeked_phase == null) {
          warn("next player")
          next_player(gameo)
          peeked_phase  = gameo.peek_phase
        }
      } else {
        warn("next player with set_next_turn")
        gameo.set_next_turn(false)
        next_player(gameo)
        peeked_phase  = gameo.peek_phase
      }
      
      
    } while ((peeked_phase != null) && (peeked_phase.phase_type.is.startsWith(RoomPhaseEnum.PROCESS.toString)))
    
    //if (peeked_phase == null)
    //  next_player(gameo)
    //else {
    val deadline = 
      if (peeked_phase.phase_type.is == RoomPhaseEnum.MAIN.toString) gameo.room.action_time.is
      else gameo.room.reaction_time.is
        
      // TODO : 這邊加入 Actioner deadline Penalty
    peeked_phase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is)).save

    gameo.room.processing(false).save
    
    if (gameo.new_round) {
      RoomActor.sendRoomMessage(gameo.room.id.is, SessionVarSet(room = gameo.room, roomround = gameo.roomround, roomphases = gameo.roomphases, 
                                userentrys = gameo.userentrys, userentryteams = gameo.userentryteams, card_list = gameo.card_list))
      gameo.add_update(List(ForceUpdateEnum.ACTION_BAR, ForceUpdateEnum.CARD_TABLE, ForceUpdateEnum.USER_TABLE, 
                            ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.TEAM_TABLE))
      RoomActor.sendRoomMessage(gameo.room.id.is, RoomForceUpdate(gameo.room.id.is, gameo.pop_update))
      
    } else {
      if (peeked_phase.phase_type.is == RoomPhaseEnum.ANGELSONG_REACTION.toString) {
        warn("ANGELSONG_REACTION START")
        warn("roomround : " + gameo.roomround)
        warn("roomphases : " + gameo.roomphases)
        warn("ANGELSONG_REACTION END")
      }
      
      RoomActor.sendRoomMessage(gameo.room.id.is, SessionVarSet(room = gameo.room, roomphases = gameo.roomphases, 
                                userentrys = gameo.userentrys, card_list = gameo.card_list))
      gameo.add_update(List(ForceUpdateEnum.ACTION_BAR, ForceUpdateEnum.CARD_TABLE, ForceUpdateEnum.USER_TABLE, 
                            ForceUpdateEnum.TIME_TABLE))
      RoomActor.sendRoomMessage(gameo.room.id.is, RoomForceUpdate(gameo.room.id.is, gameo.pop_update))
    }
    //}
  }
  
  def next_player(gameo : GameObject) : Unit = {
    val userentrys_rl = gameo.userentrys.filter(x => (!x.revoked.is) && 
                                               (x.hasnt_room_flag(UserEntryRoomFlagEnum.SKIPPED)))
    val current_player = UserEntry.get(gameo.roomround.actioner_id.is, gameo.userentrys)
    warn ("next_player actioner_id: " + gameo.roomround.actioner_id.is)
    var player_index  = userentrys_rl.map(_.id.is).indexOf(gameo.roomround.actioner_id.is)
    warn ("next_player player_index: " + player_index)
    //val roomphase     = gameo.roomphase
    //var new_phase_no = roomphase.phase_no.is + 1
    val roomround     = gameo.roomround
    var new_roomround = roomround
    var additional    = roomround.additional.is
    var additional_flag = roomround.additional_flag.is
    var new_phase_type  = RoomPhaseEnum.MAIN
    var turn_no       = roomround.turn_no.is
    var new_phases    = gameo.roomphases
    
    
    gameo.room.stack_index(-1).save
    
    if (additional <= 0) { // && (currentplayer.live.is)) {
      player_index = player_index + 1
      additional   = 0
      turn_no      = 1
      additional_flag = ""
      
      if (player_index  >= userentrys_rl.length) {
        // 新回合
        player_index = 0
        
        
        //new_phase_no = 0

        new_roomround = RoomRound.create.room_id(gameo.room.id.is).round_no(gameo.roomround.round_no.is + 1)
                                 .last_round(gameo.roomround.id.is)
                                  
        
        new_roomround.save
        gameo.roomround = new_roomround
        new_phases = List() //RoomPhase.findAllByRoomRoundId(new_roomround.id.is)
        //gameo.roomphases = new_phases
        val talk = Talk.create.roomround_id(new_roomround.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                      .message("第 " + (new_roomround.round_no.is.toString) + " 日 "+ (new java.util.Date).toString)
        talk.save
      }
    } else {
      additional = additional - 1
      turn_no = turn_no + 1
      //additional_flag = roomround.additional_flag.is
      new_phase_type =
        if ((roomround.additional_flag.is == RoomAdditionalFlagEnum.ATTACK.toString) ||
            (roomround.additional_flag.is == RoomAdditionalFlagEnum.ATTACK_AIR.toString) ||
            (roomround.additional_flag.is == RoomAdditionalFlagEnum.ATTACK_FIRE.toString))
          RoomPhaseEnum.ATTACK
        else if (roomround.additional_flag.is == RoomAdditionalFlagEnum.MAGIC.toString)
          RoomPhaseEnum.MAGIC
        else if (roomround.additional_flag.is == RoomAdditionalFlagEnum.ACTIVATE.toString)
          RoomPhaseEnum.MAIN_NO_ACTIVATE
        else if (roomround.additional_flag.is == RoomAdditionalFlagEnum.BOTH.toString)
          RoomPhaseEnum.ATTACK_OR_MAGIC
        else
          RoomPhaseEnum.MAIN
      warn("new_phase_type : " + new_phase_type.toString)
    }
          
    warn ("next_player new_player_index: " + player_index)
    val next_player = userentrys_rl(player_index)
    val next_player_role = next_player.get_role
    warn ("next_player next_player: " + next_player)
    new_roomround.actioner_id(next_player.id.is).turn_no(turn_no)
                 .additional(additional).additional_flag(additional_flag)
                 .round_flags("")
                 .save
    gameo.roomround = new_roomround
    
    if (next_player != current_player) {
      warn("Role Status Reset")
      if ((next_player_role == RoleAssassin) && (next_player.tapped.is)) {
        warn("Assassin Status Rest")
        next_player.tapped(false).hand_max(next_player.hand_max.is + 1)
      } else if (next_player_role == RoleSwordSaint) {
        warn("SwordSaint Status Rest")
        next_player.remove_role_flag(UserEntryRoleFlagEnum.SWORDSAINT_COMBO)
                   .remove_role_flag(UserEntryRoleFlagEnum.SWORDSAINT_SWORDSHADOW)
                   //.save 後面會 save
      } else if (next_player_role == RoleSaintGirl) {
        next_player.remove_role_flag(UserEntryRoleFlagEnum.SAINTGIRL_HOLYHEAL)
      } else if (next_player_role == RoleSaintLance) {
        next_player.remove_role_flag(UserEntryRoleFlagEnum.SAINTLANCE_NOSKYLANCE)
      } else if (next_player_role == RoleMagicSword) {
        next_player.remove_role_flag(UserEntryRoleFlagEnum.MAGICSWORD_DARKSTUNNED)
                   .remove_role_flag(UserEntryRoleFlagEnum.MAGICSWORD_COMBO)
                   .tapped(false)
      } else if ((next_player_role == RoleJudicator) && (next_player.tapped.is)) {
        next_player.add_yellow_index(1).save
      } else if ((next_player_role == RoleAdventurer)) {
        next_player.remove_role_flag(UserEntryRoleFlagEnum.ADVENTURER_ADDON)
                   .remove_role_flag(UserEntryRoleFlagEnum.ADVENTURER_THEFT)
      } else if ((next_player_role == RolePrayer)) {
        next_player.remove_role_flag(UserEntryRoleFlagEnum.PRAYER_MANATIDE)
      } else if (next_player_role == RoleButterfly) {
        val enemyteam = gameo.userentryteams.filter(_.id.is != next_player.team_id.is)(0)
        if (enemyteam.has_team_flag(UserEntryTeamFlagEnum.MORALGUARD))
          enemyteam.remove_team_flag(UserEntryTeamFlagEnum.MORALGUARD).save
      }
      
      if (current_player.get_role == RoleRedKnight) {
        if (current_player.tapped.is)
          current_player.add_heals(2).tapped(false)
        current_player.remove_role_flag(UserEntryRoleFlagEnum.REDKNIGHT_CONTRACT)
                      .remove_role_flag(UserEntryRoleFlagEnum.REDKNIGHT_DISCIPLINE).save
      }
    }
    
    if (next_player_role == RoleSwordSaint)
      next_player.remove_role_flag(UserEntryRoleFlagEnum.SWORDSAINT_DONE)
    else if (next_player_role == RoleSaintLance)
      next_player.remove_role_flag(UserEntryRoleFlagEnum.SAINTLANCE_NOSMITE)
    else if (next_player_role == RoleElementalist)
      next_player.remove_role_flag(UserEntryRoleFlagEnum.ELEMENTALIST_IGNITE)
    else if (next_player_role == RoleSwordEmp)
      next_player.remove_role_flag(UserEntryRoleFlagEnum.SWORDEMP_LIBIDINALWILL_DONE)
    
    next_player.remove_user_flag(UserEntryFlagEnum.FASTBLESSED)
               .remove_role_flag(UserEntryRoleFlagEnum.ATTACK)
               .remove_role_flag(UserEntryRoleFlagEnum.MAGIC) 
               .remove_role_flag(UserEntryRoleFlagEnum.SPECIAL).save 
    
    if (current_player.get_role == RoleNecromancer) {
      if (current_player.has_role_flag(UserEntryRoleFlagEnum.NECROMANCER_MAGIC))
        current_player.add_heals(1).remove_role_flag(UserEntryRoleFlagEnum.NECROMANCER_MAGIC).save
    } else if (current_player.get_role == RolePrayer) 
      current_player.remove_role_flag(UserEntryRoleFlagEnum.PRAYER_MAGIC).save
    //currentplayer.save
    
    
    //gameo.roomphases = List()
    gameo.roomphases = new_phases
    val new_phase = RoomPhase.create.roomround_id(new_roomround.id.is)
                             .phase_type(new_phase_type.toString).actioner_id(next_player.id.is)
                             .deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.action_time.is))
                             
    gameo.push_phase(new_phase)
    gameo.room.processing(false).save
    
    //new_phases = RoomPhase.findAllByRoomRoundId(new_roomround.id.is)
    //gameo.roomphases = new_phases
    
    // 檢查中毒與虛弱
    val cards_in_front = CardPool.in_front(next_player, gameo.card_list)
    val fiveseal_targets = gameo.userentrys.filter(_.get_role == RoleSealer).map(_.target_user.is)
    if (fiveseal_targets.contains(next_player.id.is)) {
      val weaken_phase = 
        RoomPhase.create.roomround_id(new_roomround.id.is)
                        .phase_type(RoomPhaseEnum.FIVESEAL_REACTION.toString).actioner_id(next_player.id.is)
                        .power(math.min(4, 2 + gameo.card_list.filter{ x =>
                              val card_data = x.to_card
                              (x.room_id.is == gameo.room.id.is) &&
                              (x.position.is == CardPositionEnum.FRONT.toString) &&
                              (x.to_card.has_action(MTypeEnum.ACTION_SEALER_SEAL))}.length))
                        .last_phase_type(RoomPhaseEnum.FIVESEAL_REACTION.toString)
                        .deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))

      gameo.push_phase(weaken_phase)
    }
    
    val weakens_in_front = cards_in_front.filter(_.to_card.cardmagic_enum == CardMagicEnum.WEAKEN)
    if (weakens_in_front.length != 0) {
      val weaken_phase = 
        RoomPhase.create.roomround_id(new_roomround.id.is)
                        .phase_type(RoomPhaseEnum.WEAKEN_REACTION.toString).actioner_id(next_player.id.is).action_card(weakens_in_front(0).id.is)
                        .power(3).last_phase_type(RoomPhaseEnum.WEAKEN_REACTION.toString)
                        .deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
        gameo.push_phase(weaken_phase)
      //new_phases = RoomPhase.findAllByRoomRoundId(new_roomround.id.is)
      //gameo.roomphases = new_phases
    }
    
    if ((next_player.get_role == RoleBrave) && (next_player != current_player) && 
        (next_player.tapped.is)) {
      next_player.tapped(false).fixed_hand_max(0).save
      
      val forbidden_selfdamage_phase = 
        RoomPhase.create.roomround_id(new_roomround.id.is)
                        .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                        .actioner_id(next_player.id.is).actionee_id(next_player.id.is).power(3)
                        .deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
        gameo.push_phase(forbidden_selfdamage_phase)
    }
    
    if ((next_player.get_role == RoleMiko) && (next_player != current_player) && 
        (next_player.tapped.is)) {
      
      val miko_selfdamage_phase = 
        RoomPhase.create.roomround_id(new_roomround.id.is)
                        .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                        .actioner_id(next_player.id.is).actionee_id(next_player.id.is).power(1)
                        .deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
        gameo.push_phase(miko_selfdamage_phase)
    }
      
    val poisons_in_front = cards_in_front.filter(_.to_card.cardmagic_enum == CardMagicEnum.POISON)
    if (poisons_in_front.length != 0 ) {
      poisons_in_front.foreach { poison_in_front =>
        //val old_owner = UserEntry.get(poison_in_front.owner_id.is, gameo.userentrys)
        val old_owner = poison_in_front.owner_id.is
        //if (old_owner.get_role == RoleElementalist)
        //  old_owner.add_yellow_index(1).save
        
        //poison_in_front.discard(gameo)
        //val card = CardHelper.draw_card(gameo.room, gameo.card_list)
        //card.owner_id(next_player.id.is).position(CardPositionEnum.HAND.toString).save
        val poison_phase = 
          RoomPhase.create.roomround_id(new_roomround.id.is)
                          .phase_type(RoomPhaseEnum.PROCESS_POISON.toString).action_card(poison_in_front.id.is)
                          .actioner_id(old_owner).actionee_id(next_player.id.is).power(1)
                          .deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
          gameo.push_phase(poison_phase)
      }
      
    if ((next_player.get_role == RoleAngel) && (next_player.gems.is + next_player.crystals.is > 0)) {
      val angelsong_phase = 
        RoomPhase.create.roomround_id(new_roomround.id.is)
                        .phase_type(RoomPhaseEnum.ANGELSONG_REACTION.toString).actioner_id(next_player.id.is)
                        .deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.action_time.is))
      gameo.push_phase(angelsong_phase)
    }

      //gameo.room.save
      //RoomActor.sendRoomMessage(gameo.room.id.is, SessionVarSet(room = gameo.room, roomround = new_roomround, roomphases = new_phases, userentrys = gameo.userentrys,
      //                                                          userentryteams = gameo.userentryteams, card_list = gameo.card_list))
      //RoomActor.sendRoomMessage(gameo.room.id.is, RoomForceUpdate(gameo.room.id.is, List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.TIME_TABLE, 
      //                                                                                   ForceUpdateEnum.TEAM_TABLE, ForceUpdateEnum.ACTION_BAR, ForceUpdateEnum.CARD_TABLE)))
      //gameo.add_update(List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.TIME_TABLE, 
      //                      ForceUpdateEnum.TEAM_TABLE, ForceUpdateEnum.ACTION_BAR, ForceUpdateEnum.CARD_TABLE))
      
      //val actioner = UserEntry.get(next_phase.player.is, gameo.userentrys)
      //val actionee = UserEntry.get(next_phase.target_player.is, gameo.userentrys)
      //process_check_heal(gameo, next_player, next_player)
      //GameProcessor.next_phase(gameo)
      //return
      
      /*
      val poison_talk = Talk.create.roomround_id(new_roomround.id.is).mtype(MTypeEnum.RESULT_POISON.toString)
                            .actioner_id(next_player.id.is).message_flags(poisons_in_front.length.toString)
      poison_talk.save
      poison_talk.send(gameo.room)
      
      val cards_in_hand = CardPool.in_hand(next_player, gameo.card_list)
      if (cards_in_hand.length > next_player.get_hand_max) {
        val new_phase = RoomPhase.create.roomround_id(new_roomround.id.is) //.card_enum(roomphase.card_enum.is).power(roomphase.power.is)
                                 .phase_type(RoomPhaseEnum.DISCARD_REACTION.toString).player(next_player.id.is)
                                 .deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
        //RoomPhase.push_before(gameo, new_phase)
        //new_phases = RoomPhase.findAllByRoomRoundId(new_roomround.id.is)
      }
      */
    }

    
    //if (!GameProcessor.check_victory(gameo)) {
    
    gameo.set_new_round(true)
    /*
    RoomActor.sendRoomMessage(gameo.room.id.is, SessionVarSet(room = gameo.room, roomround = gameo.roomround, roomphases = gameo.roomphases, userentrys = gameo.userentrys,
                                                              userentryteams = gameo.userentryteams, card_list = gameo.card_list))
    
    gameo.add_update(List(ForceUpdateEnum.TEAM_TABLE, ForceUpdateEnum.TALK_TABLE))
    */
    //if (poisons_in_front.length != 0)
    //  gameo.add_update(List(ForceUpdateEnum.TEAM_TABLE, ForceUpdateEnum.TALK_TABLE))
      //RoomActor.sendRoomMessage(gameo.room.id.is, RoomForceUpdate(gameo.room.id.is, List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.TIME_TABLE, 
      //                                                                                   ForceUpdateEnum.TEAM_TABLE, ForceUpdateEnum.ACTION_BAR, ForceUpdateEnum.CARD_TABLE)))
    //else
    //  RoomActor.sendRoomMessage(gameo.room.id.is, RoomForceUpdate(gameo.room.id.is, List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.TIME_TABLE, 
    //                                                                                     ForceUpdateEnum.TEAM_TABLE, ForceUpdateEnum.ACTION_BAR, ForceUpdateEnum.CARD_TABLE)))
    //}
  }
  
  def process_attack(gameo : GameObject, action : Action, card : CardPool) = {
    val actioner_id   = action.actioner_id.is
    val actioner : UserEntry = UserEntry.get(actioner_id, gameo.userentrys)
    //val action_enum   = MTypeEnum.get_action(action.mtype.is)
    val userentrys    = gameo.userentrys
    val userentrys_rr = UserEntry.rr(userentrys)
    val actionee_id = action.actionee_id.is
    val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        
    //val actionee_shield_in_front = CardPool.in_front_shield(actionee, gameo.card_list)
    val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
    
    val is_auto_hit = {
      if ((actioner.get_role == RoleSwordSaint) && (gameo.roomround.turn_no.is == 3))
        true
      else if ((actioner.get_role == RoleBerserker) && (card.to_card.has_action(MTypeEnum.ACTION_BERSERKER_BLOODROAR)) &&
        (actionee.heals.is == 2))
        true
      else if ((actioner.get_role == RoleArcher) && (action.action_flags2.is == MTypeEnum.ACTION_ARCHER_AIM.toString))
        true
      //else if ((actioner.get_role == RoleAssassin) && (actioner.tapped.is) && (roomphase.phase_type.is == RoomPhaseEnum.ATTACK.toString))
      //  true
      else
        false
    }
    
    if (is_auto_hit) {
      process_endure(gameo, actioner, actionee, is_auto_hit)
    } else {  
      val new_phase = RoomPhase.createFrom(roomphase).card_enum(card.to_card.card_enum.toString) //.roomround_id(gameo.roomround.id.is)
                               //.power(roomphase.power.is)
                               .phase_type(RoomPhaseEnum.ATTACK_REACTION.toString)
                               .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
                               .last_phase_type(roomphase.phase_type.is).last_phase_subtype(roomphase.phase_subtype.is)
                               .phase_flags(roomphase.phase_type.is)
      RoomPhase.push(gameo, new_phase)
    }
  }
  
  def process_deceive_attack(gameo : GameObject, action : Action, attr : String) {
    val actioner_id   = action.actioner_id.is
    val actioner : UserEntry = UserEntry.get(actioner_id, gameo.userentrys)
    //val action_enum   = MTypeEnum.get_action(action.mtype.is)
    val userentrys    = gameo.userentrys
    val userentrys_rr = UserEntry.rr(userentrys)
    val actionee_id = action.actionee_id.is
    val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        
    //val actionee_shield_in_front = CardPool.in_front_shield(actionee, gameo.card_list)
    val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
    
    val new_phase = RoomPhase.createFrom(roomphase).card_enum(attr) //.roomround_id(gameo.roomround.id.is)
                             //.power(roomphase.power.is)
                             .phase_type(RoomPhaseEnum.ATTACK_REACTION.toString)
                             .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
                             .last_phase_type(roomphase.phase_type.is).last_phase_subtype(roomphase.phase_subtype.is)
                             .phase_flags(roomphase.phase_type.is)
    RoomPhase.push(gameo, new_phase)
    
  }
  
  // 不包含被迎擊
  def process_attack_miss(gameo : GameObject, actioner : UserEntry, actionee : UserEntry) = {
    val roomphase = gameo.roomphase
    val target_player = UserEntry.get(roomphase.actionee_id.is, gameo.userentrys)
      if ((target_player.get_role == RoleArcher) &&
          (roomphase.last_phase_type.is == RoomPhaseEnum.ATTACK.toString)) {
        val penetrate_phase = RoomPhase.createFrom(roomphase)
                                      .phase_type(RoomPhaseEnum.PENETRATE_REACTION.toString)
                                      .actioner_id(target_player.id.is).actionee_id(actionee.id.is)
                                      //.deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
                                      //.last_phase_type(roomphase.phase_type.is).last_phase_subtype(roomphase.phase_subtype.is)
                                     //.phase_flags(action.id.is.toString)
        RoomPhase.push(gameo, penetrate_phase)
      } else if ((target_player.get_role == RoleSwordEmp) &&
                  (roomphase.last_phase_type.is == RoomPhaseEnum.ATTACK.toString)) {
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
      } else if ((target_player.get_role == RoleMonk) &&
                  (roomphase.last_phase_type.is == RoomPhaseEnum.ATTACK.toString)) {
        if (target_player.has_role_flag(UserEntryRoleFlagEnum.MONK_POWERUP)) {
          val process_damage_phase1 = RoomPhase.createFrom(roomphase)
                                         .phase_type(RoomPhaseEnum.PROCESS_DAMAGE.toString)
                                         .actioner_id(target_player.id.is).actionee_id(target_player.id.is)
                                         .power(target_player.yellow_index.is) //.card_enum(card.to_card.card_enum.toString)
                                         .last_phase_type("")
          RoomPhase.push(gameo, process_damage_phase1)
        }
        //target_player.save
      } else if ((target_player.get_role == RoleBrave) &&
                  (roomphase.last_phase_type.is == RoomPhaseEnum.ATTACK.toString)) {
        if (target_player.has_role_flag(UserEntryRoleFlagEnum.BRAVE_ROAR)) {
          target_player.add_blue_index(1).save
        }
        if (target_player.crystals.is + target_player.gems.is > 0) {
          val forbidden_phase = RoomPhase.createFrom(roomphase)
                                        .phase_type(RoomPhaseEnum.FORBIDDEN_REACTION.toString)
                                        .actioner_id(target_player.id.is).actionee_id(actioner.id.is)
                                        .last_phase_type("")
          RoomPhase.push(gameo, forbidden_phase)
        }
        //target_player.save
      }
  }
  
  // endure 還是攻方是 actioner
  def process_endure(gameo : GameObject, actioner : UserEntry, actionee : UserEntry, is_auto_hit : Boolean = false) = {
    val userentrys    = gameo.userentrys
    val userentrys_rr = UserEntry.rr(userentrys)
    val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
    
    //val actioner : UserEntry = UserEntry.get(roomphase.target_player.is, userentrys)
    //val actionee_id = action.actioner_id.is   // Note : 這邊換成用 actioner
    //val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        
    val actionee_shield_in_front = CardPool.in_front_shield(actionee, gameo.card_list)
    
    val is_ignore_shield =
      (actioner.get_role == RoleSwordSaint) && (CardEnum.get_card(roomphase.card_enum).has_action(MTypeEnum.ACTION_SWORDSAINT_STRONGGALE)) ||
      is_auto_hit
    
    
    if ((!is_ignore_shield) && (actionee_shield_in_front.length != 0)) {
      // 目標以聖盾抵擋
      println("Process Endure - Shield")
      val shield_card = actionee_shield_in_front(0)
      shield_card.discard(gameo)
          
      val talk = Talk.create.roomround_id(gameo.roomround.id.is).mtype(MTypeEnum.ACTION_SHIELD.toString)
                     .actioner_id(actionee.id.is)
      talk.save
      talk.send(gameo.room)
      
      process_attack_miss(gameo, actioner, actionee)
      /*
      val target_player = UserEntry.get(roomphase.actionee_id.is, gameo.userentrys)
      if ((target_player.get_role == RoleArcher) &&
          (roomphase.last_phase_type.is == RoomPhaseEnum.ATTACK.toString)) {
        val penetrate_phase = RoomPhase.createFrom(roomphase)
                                      .phase_type(RoomPhaseEnum.PENETRATE_REACTION.toString)
                                      .actioner_id(target_player.id.is).actionee_id(actionee.id.is)
                                      //.deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
                                      //.last_phase_type(roomphase.phase_type.is).last_phase_subtype(roomphase.phase_subtype.is)
                                     //.phase_flags(action.id.is.toString)
        RoomPhase.push(gameo, penetrate_phase)
      } else if ((target_player.get_role == RoleSwordEmp) &&
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
      } */
    } else if ((actioner.get_role == RoleBerserker) && (actioner.gems.is > 0) &&
               ((roomphase.last_phase_type.is == RoomPhaseEnum.ATTACK.toString) ||
                (roomphase.last_phase_type.is == RoomPhaseEnum.REATTACK.toString))) {
      val new_phase = RoomPhase.createFrom(roomphase) //.roomround_id(gameo.roomround.id.is)
                               .card_enum(roomphase.card_enum.is)
                               //.phase_flags(roomphase.phase_flags.is)
                               .phase_type(RoomPhaseEnum.LACERATE_REACTION.toString)
                               .actioner_id(actioner.id.is).actionee_id(actionee.id.is)
                               //.deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
      RoomPhase.push(gameo, new_phase)
    } else if ((actioner.get_role == RoleSaintLance) &&
               ((roomphase.last_phase_type.is == RoomPhaseEnum.ATTACK.toString) ||
                (roomphase.last_phase_type.is == RoomPhaseEnum.REATTACK.toString))) {
      val new_phase = RoomPhase.createFrom(roomphase) //.roomround_id(gameo.roomround.id.is)
                               .card_enum(roomphase.card_enum.is)
                               //.phase_flags(roomphase.phase_flags.is)
                               .phase_type(RoomPhaseEnum.SMITE_REACTION.toString)
                               .actioner_id(actioner.id.is).actionee_id(actionee.id.is)
                               //.deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
      RoomPhase.push(gameo, new_phase)
    } else if ((actioner.get_role == RoleRuneMage) &&
               ((roomphase.last_phase_type.is == RoomPhaseEnum.ATTACK.toString))) {
      val new_phase = RoomPhase.createFrom(roomphase) //.roomround_id(gameo.roomround.id.is)
                               .card_enum(roomphase.card_enum.is)
                               //.phase_flags(roomphase.phase_flags.is)
                               .phase_type(RoomPhaseEnum.GHOSTS100_REACTION.toString)
                               .actioner_id(actioner.id.is).actionee_id(actionee.id.is)
                               //.deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
      RoomPhase.push(gameo, new_phase)
    } else if ((actioner.get_role == RoleSwordEmp) &&
               ((roomphase.last_phase_type.is == RoomPhaseEnum.ATTACK.toString))) {
      if (actioner.has_role_flag(UserEntryRoleFlagEnum.SWORDEMP_ANGELSOUL)) {
        actioner.add_heals(2).save
      } else if (actioner.has_role_flag(UserEntryRoleFlagEnum.SWORDEMP_DEVILSOUL)) {
        roomphase.power(roomphase.power.is + 1).save
      }
      val new_phase = RoomPhase.createFrom(roomphase) //.roomround_id(gameo.roomround.id.is)
                               .card_enum(roomphase.card_enum.is)
                               //.phase_flags(roomphase.phase_flags.is)
                               .phase_type(RoomPhaseEnum.SWORDKI_REACTION.toString)
                               .actioner_id(actioner.id.is).actionee_id(actionee.id.is)
                               //.deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
      RoomPhase.push(gameo, new_phase)
    } else if ((actioner.get_role == RoleBrave) &&
               (roomphase.last_phase_type.is == RoomPhaseEnum.ATTACK.toString) &&
               (actioner.crystals.is + actioner.gems.is > 0)) {
      val forbidden_phase = RoomPhase.createFrom(roomphase)
                                     .phase_type(RoomPhaseEnum.FORBIDDEN_REACTION.toString)
                                     .actioner_id(actioner.id.is).actionee_id(actionee.id.is)
                                     //.last_phase_type("")
       RoomPhase.push(gameo, forbidden_phase)
    } else if ((actioner.get_role == RoleRedKnight) && (actioner.yellow_index.is > 0) &&
               (roomphase.last_phase_type.is == RoomPhaseEnum.ATTACK.toString)) {
      val new_phase = RoomPhase.createFrom(roomphase) //.roomround_id(gameo.roomround.id.is)
                               .card_enum(roomphase.card_enum.is)
                               //.phase_flags(roomphase.phase_flags.is)
                               .phase_type(RoomPhaseEnum.BLOODFEAST_REACTION.toString)
                               .actioner_id(actioner.id.is).actionee_id(actionee.id.is)
                               //.deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
      RoomPhase.push(gameo, new_phase)
    } else {
      if ((actioner.get_role == RoleMagicSword) && (actioner.has_role_flag(UserEntryRoleFlagEnum.MAGICSWORD_DARKSTUN))) {
        val cards_in_hand = CardPool.in_hand(actioner, gameo.card_list)
        val draw_num      = actioner.get_hand_max - cards_in_hand.length
        process_drawcard(gameo, actioner, draw_num)
      }
      
      process_endure2(gameo, actioner, actionee)
      //gameo.refresh_roomphase
    }
  }
  
  def process_endure2(gameo : GameObject, actioner : UserEntry, actionee : UserEntry) = {
    val roomphase = gameo.roomphase
    val powerbless_cards = CardPool.in_front(actioner, gameo.card_list)
                                   .filter(_.to_card.has_action(MTypeEnum.ACTION_PRAYER_POWERBLESS))
    
    if ((powerbless_cards.length != 0)  &&
        ((roomphase.last_phase_type.is == RoomPhaseEnum.ATTACK.toString) ||
         (roomphase.last_phase_type.is == RoomPhaseEnum.REATTACK.toString))) {
      val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
      val new_phase = RoomPhase.createFrom(roomphase) //.roomround_id(gameo.roomround.id.is)
                               .card_enum(roomphase.card_enum.is)
                               //.phase_flags(roomphase.phase_flags.is)
                               .phase_type(RoomPhaseEnum.POWERBLESS_REACTION.toString)
                               .actioner_id(actioner.id.is).actionee_id(actionee.id.is)
                               //.deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
      RoomPhase.push(gameo, new_phase)
    } else {
      process_check_heal(gameo, actioner, actionee)
    }
  }
  
  
  def process_check_heal(gameo : GameObject, actioner : UserEntry, actionee : UserEntry) = {
    val roomphase = gameo.roomphase
    println("Process Check Heal")
    if ((actionee.get_role == RoleMonk) && (roomphase.power.is > 4))
      roomphase.power(4).save
    
    // 目標選擇治療與否
    if ((actionee.heals.is != 0) &&
        ((actionee.get_role != RoleNecromancer) ||
         ((roomphase.last_phase_type.is != RoomPhaseEnum.ATTACK.toString) &&
          (roomphase.last_phase_type.is != RoomPhaseEnum.REATTACK.toString))) &&
       ((actionee.get_role != RoleRedKnight) ||
         (actioner == actionee))) {
      val new_phase = RoomPhase.createFrom(roomphase) //.roomround_id(gameo.roomround.id.is)
                               //.card_enum(roomphase.card_enum.is)
                               .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
                               //.power(roomphase.power.is)
                               .phase_type(RoomPhaseEnum.HEAL_REACTION.toString)
                               //.last_phase_type(roomphase.phase_type.is).last_phase_subtype(roomphase.phase_subtype.is)
      gameo.push_phase(new_phase)
      //gameo.refresh_roomphase
    } else {
      println("Process Damage")
      process_post_heal(gameo, actioner, actionee)
    }
  }
  
  def process_post_heal(gameo : GameObject, actioner : UserEntry, actionee : UserEntry) = {
    val roomphase = gameo.roomphase
    val butterflys = gameo.userentrys.filter(_.get_role == RoleButterfly)
    if ((butterflys.length != 0) && (roomphase.last_phase_type.is == "") &&
        (roomphase.last_phase_type.is != RoomPhaseEnum.ATTACK.toString) && 
        (roomphase.last_phase_type.is != RoomPhaseEnum.REATTACK.toString) &&
        (roomphase.last_phase_type.is != RoomPhaseEnum.WEAKEN_REACTION.toString) && 
        (roomphase.power.is >= 1) && (roomphase.power.is <= 2)) {
      val butterfly = butterflys(0)
      val butterfly_back_cards = CardPool.in_back(butterfly, gameo.card_list)
      if ((roomphase.power.is == 1) && (butterfly_back_cards.length >= 1)) {
        val new_phase = RoomPhase.createFrom(roomphase) //.roomround_id(gameo.roomround.id.is).card_enum(roomphase.card_enum.is)
                                 .phase_type(RoomPhaseEnum.POISONPOWDER_REACTION.toString)
                                 .actioner_id(butterfly.id.is).actionee_id(actioner.id.is)
                                 .actionee2_id(actionee.id.is) 
        RoomPhase.push(gameo, new_phase)
      } else if ((roomphase.power.is == 2) && (butterfly_back_cards.length >= 2)) {
        val new_phase = RoomPhase.createFrom(roomphase) //.roomround_id(gameo.roomround.id.is).card_enum(roomphase.card_enum.is)
                                 .phase_type(RoomPhaseEnum.MIRRORFLOWER_REACTION.toString)
                                 .actioner_id(butterfly.id.is).actionee_id(actioner.id.is)
                                 .actionee2_id(actionee.id.is)
        RoomPhase.push(gameo, new_phase)
      } else
        process_damage_pre(gameo, actioner, actionee)
    } else
      process_damage_pre(gameo, actioner, actionee)
  }
  
  def process_damage_pre (gameo : GameObject, 
                    actioner : UserEntry, actionee : UserEntry) = {
    val roomphase = gameo.roomphase
    if ((actionee.get_role == RoleAssassin) && (roomphase.power.is > 0)){
      //val roomphase = gameo.roomphase
      val new_phase = RoomPhase.createFrom(roomphase) //.roomround_id(gameo.roomround.id.is).card_enum(roomphase.card_enum.is)
                               //.power(roomphase.power.is).phase_flags(roomphase.phase_flags.is)
                               .phase_type(RoomPhaseEnum.WATERSHADOW_REACTION.toString)
                               .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
      RoomPhase.push(gameo, new_phase)
    } else if ((actionee.get_role == RoleButterfly) && (roomphase.power.is > 0) &&
               (CardPool.in_back(actionee, gameo.card_list).length > 0)) {
      //val roomphase = gameo.roomphase
      val new_phase = RoomPhase.createFrom(roomphase) //.roomround_id(gameo.roomround.id.is).card_enum(roomphase.card_enum.is)
                               //.power(roomphase.power.is).phase_flags(roomphase.phase_flags.is)
                               .phase_type(RoomPhaseEnum.PILGRIMAGE_REACTION.toString)
                               .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
      RoomPhase.push(gameo, new_phase)
    } else {
      if ((actionee.get_role == RoleSage) && (roomphase.power.is == 1) && 
            (roomphase.last_phase_type.is != RoomPhaseEnum.ATTACK.toString) && 
            (roomphase.last_phase_type.is != RoomPhaseEnum.REATTACK.toString) &&
            (roomphase.last_phase_type.is != RoomPhaseEnum.WEAKEN_REACTION.toString)) {
        //val roomphase = gameo.roomphase
        val new_phase = RoomPhase.createFrom(roomphase) //.roomround_id(gameo.roomround.id.is).card_enum(roomphase.card_enum.is)
                                 //.power(roomphase.power.is).phase_flags(roomphase.phase_flags.is)
                                 .phase_type(RoomPhaseEnum.REFLECT_REACTION.toString)
                                 .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
        RoomPhase.push(gameo, new_phase)
      } 
      process_damage_pre2(gameo, actioner, actionee)
    } 
  }
  
  def process_damage_pre2 (gameo : GameObject, 
                    actioner : UserEntry, actionee : UserEntry) = {
    val roomphase = gameo.roomphase
    if ((actionee.get_role == RoleSoulMage) && (actionee.target_user.is != 0) &&
        (actionee.blue_index.is > 0) && (roomphase.power.is > 0)){
      val new_phase = RoomPhase.createFrom(roomphase) //.roomround_id(gameo.roomround.id.is).card_enum(roomphase.card_enum.is)
                                 //.power(roomphase.power.is).phase_flags(roomphase.phase_flags.is)
                               .phase_type(RoomPhaseEnum.SOULLINK_REACTION.toString)
                               .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
                               .phase_flags(SoulMageEnum.FROMMAGE.toString)
      RoomPhase.push(gameo, new_phase)
    } else if (roomphase.power.is > 0) {
      val soulmages = gameo.userentrys.filter(x =>
       ((x.get_role == RoleSoulMage) && (x.target_user.is == actionee.id.is) &&
        (x.blue_index.is > 0)))
      if (soulmages.length != 0 ) {
        val new_phase = RoomPhase.createFrom(roomphase) //.roomround_id(gameo.roomround.id.is).card_enum(roomphase.card_enum.is)
                                   //.power(roomphase.power.is).phase_flags(roomphase.phase_flags.is)
                                 .phase_type(RoomPhaseEnum.SOULLINK_REACTION.toString)
                                 .actioner_id(soulmages(0).id.is).actionee_id(actioner.id.is)
                                 .phase_flags(SoulMageEnum.TOMAGE.toString)
        RoomPhase.push(gameo, new_phase)
      } else
        process_damage(gameo, actioner, actionee)
    } else
      process_damage(gameo, actioner, actionee)
  }
  
  def process_damage(gameo : GameObject, 
                    actioner : UserEntry, actionee : UserEntry) = {
    //val actioner_id   = action.actioner_id.is
    //val actioner : UserEntry = UserEntry.get(actioner_id, gameo.userentrys)
    //val action_enum   = MTypeEnum.get_action(action.mtype.is)
    //val userentrys    = gameo.userentrys
    //val userentrys_rr = UserEntry.rr(userentrys)
    //val actionee_id = action.actionee_id.is
    //val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        
    //val actionee_shield_in_front = CardPool.in_front_shield(actionee, gameo.card_list)
    val roomphase = 
      //if (roomphase_in == null)
        RoomPhase.get_phase(gameo.room, gameo.roomphases)
      //else roomphase_in
    
    // 目標受傷害
    val (message_type, is_attack) =
      if (roomphase.last_phase_type.is == RoomPhaseEnum.ATTACK.toString) 
        (MTypeEnum.RESULT_ATTACK, true)
      else if (roomphase.last_phase_type.is == RoomPhaseEnum.REATTACK.toString)
        (MTypeEnum.RESULT_REATTACK, true)
      else if (roomphase.last_phase_type.is == RoomPhaseEnum.WEAKEN_REACTION.toString)
        (MTypeEnum.RESULT_WEAKEN, false)
      else  
        (MTypeEnum.RESULT_MAGIC, false)
      
    val is_weaken = (roomphase.last_phase_type.is == RoomPhaseEnum.WEAKEN_REACTION.toString)
      
    val talk = Talk.create.roomround_id(gameo.roomround.id.is).mtype(message_type.toString)
                   .actioner_id(actionee.id.is).message_flags(roomphase.power.is.toString)
    talk.save
    talk.send(gameo.room)
    
    if ((!is_attack) && (!is_weaken) && (actioner.get_role == RoleElementalist) &&
        (actioner.hasnt_role_flag(UserEntryRoleFlagEnum.ELEMENTALIST_IGNITE))) {
      actioner.add_yellow_index(1).save
    } else if ((!is_attack) && (!is_weaken) && (actionee.get_role == RoleSage) &&
        (roomphase.power.is > 3)) {
      actionee.add_gems(2).save
    } else if ((!is_attack) && (!is_weaken) && (actionee.get_role == RoleBrave) &&
        (roomphase.power.is > 0) && (actionee.gems.is > 0)) {
      //actionee.add_yellow_index(3).save
      val new_phase = RoomPhase.createFrom(roomphase) //.roomround_id(gameo.roomround.id.is).card_enum(roomphase.card_enum.is)
                                 //.power(roomphase.power.is).phase_flags(roomphase.phase_flags.is)
                                 .phase_type(RoomPhaseEnum.DEATHMATCH_REACTION.toString)
                                 .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
      RoomPhase.push(gameo, new_phase)
    }
    
    if ((actionee.get_role == RoleJudicator) && (!is_weaken) && (roomphase.power.is > 0))
      actionee.add_yellow_index(1).save
    
    // 獲得一寶石或水晶
    if ((is_attack)) { //&& (roomphase.power.is != 0)) {
      val actionerteam = UserEntryTeam.get(actioner.team_id, gameo.userentryteams)
      if (actionerteam.crystals.is + actionerteam.gems.is < 5) {
        if (roomphase.last_phase_type.is == RoomPhaseEnum.REATTACK.toString)
          actionerteam.crystals(actionerteam.crystals.is + 1).save
        else
          actionerteam.gems(actionerteam.gems.is + 1).save
      }
    }
      
    // 目標翻牌
    process_drawcard(gameo, actionee, roomphase.power.is, false)
    
    //
    if (is_attack && (actionee.get_role == RoleAssassin)) {
      val retaliate_talk = Talk.create.roomround_id(gameo.roomround.id.is).mtype(MTypeEnum.RESULT_RETALIATE.toString)
                                     .actioner_id(actioner.id.is)
      retaliate_talk.save
      retaliate_talk.send(gameo.room)
      process_drawcard(gameo, actioner, 1)
    }
    
    /*
    for (i <- 0 until roomphase.power.is) {
      val card = CardHelper.draw_card(gameo.room, gameo.card_list)
      card.owner_id(actionee.id.is).position(CardPositionEnum.HAND.toString).save
    }
    gameo.room.save
      
    val actionee_card_in_hand = CardPool.in_hand(actionee, gameo.card_list)
    if (actionee_card_in_hand.length > actionee.hand_max.is) {
      val new_phase = RoomPhase.create.roomround_id(gameo.roomround.id.is).card_enum(roomphase.card_enum.is).power(roomphase.power.is)
                               .phase_type(RoomPhaseEnum.DISCARD_REACTION.toString).player(actionee.id.is)
                               .deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
      RoomPhase.push_after(gameo.room, new_phase, gameo.roomphases)
    }
    
    RoomActor.sendRoomMessage(gameo.room.id.is, SessionVarSet(room = gameo.room, userentryteams = gameo.userentryteams, card_list = gameo.card_list))
    gameo.add_update(List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TEAM_TABLE, ForceUpdateEnum.CARD_TABLE))
    */
  }
  
  def process_drawcard(gameo : GameObject, userentry : UserEntry, card_num : Int, is_weaken : Boolean = true) = {
    warn("process_drawcard : " + card_num)
    for (i <- 0 until card_num) {
      val card = CardHelper.draw_card(gameo)
      card.owner_id(userentry.id.is).position(CardPositionEnum.HAND.toString).save
    }
    gameo.room.save
    
    //val roomphase = gameo.roomphase
    
    /*
    val actionee_card_in_hand = CardPool.in_hand(userentry, gameo.card_list)
    if (actionee_card_in_hand.length > userentry.hand_max.is) {
      val new_phase = RoomPhase.create.roomround_id(gameo.roomround.id.is).card_enum(roomphase.card_enum.is).power(roomphase.power.is)
                               .phase_type(RoomPhaseEnum.DISCARD_REACTION.toString).player(userentry.id.is)
                               .deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
      RoomPhase.push_after(gameo.room, new_phase, gameo.roomphases)
    }
    */
    check_hand_max(gameo, userentry, is_weaken)
    
    //RoomActor.sendRoomMessage(gameo.room.id.is, SessionVarSet(room = gameo.room, userentryteams = gameo.userentryteams, card_list = gameo.card_list))
    //gameo.add_update(List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TEAM_TABLE, ForceUpdateEnum.CARD_TABLE))
    warn("process_drawcard Done")
  }
  
  def check_hand_max(gameo : GameObject, userentry : UserEntry, is_weaken : Boolean = true) = {
    val actionee_card_in_hand = CardPool.in_hand(userentry, gameo.card_list)
    val roomphase = gameo.roomphase
    if (actionee_card_in_hand.length > userentry.get_hand_max) {
      val new_phase = RoomPhase.createFrom(roomphase).card_enum(roomphase.card_enum.is) // roomround_id(gameo.roomround.id.is).
                               //.power(roomphase.power.is).last_phase_type(roomphase.phase_type.is).last_phase_subtype(roomphase.phase_subtype.is)
                               .phase_type(RoomPhaseEnum.DISCARD_REACTION.toString).actioner_id(userentry.id.is)
      if (is_weaken)
        new_phase.last_phase_type(RoomPhaseEnum.WEAKEN_REACTION.toString)
      gameo.push_phase(new_phase)
    }
  }
  
  def check_miko_tap(gameo : GameObject, userentry : UserEntry) : Unit = {
    if ((userentry.get_role != RoleMiko) || (userentry.tapped.is))
      return
    
    if (userentry.target_user.is == 0) {
      userentry.tapped(true).add_heals(1).save
      return
    }
    
    userentry.tapped(true).add_heals(1).hand_max(userentry.hand_max.is + 3).save
    
    if (userentry.target_user.is != 0) {
      val actionee = UserEntry.get(userentry.target_user.is, gameo.userentrys)
      actionee.hand_max(actionee.hand_max.is + 3).save
    }
  }
  
  def check_miko_untap(gameo : GameObject, userentry : UserEntry) : Unit = {
    if ((userentry.get_role != RoleMiko) || (!userentry.tapped.is))
      return
    
    if (userentry.target_user.is == 0)
      return
    
    userentry.tapped(false).hand_max(userentry.hand_max.is - 3).save
    if (userentry.target_user.is != 0) {
      val actionee = UserEntry.get(userentry.target_user.is, gameo.userentrys)
      actionee.hand_max(actionee.hand_max.is - 3).save
      check_hand_max(gameo, actionee)
    }
    check_hand_max(gameo, userentry)
  }
  
  def process_mbolt(gameo : GameObject, action : Action, card : CardPool) = {
    val actioner_id   = action.actioner_id.is
    val actioner : UserEntry = UserEntry.get(actioner_id, gameo.userentrys)
    //val action_enum   = MTypeEnum.get_action(action.mtype.is)
    val userentrys    = gameo.userentrys
    val userentrys_rr = UserEntry.rr(userentrys)
    val actionee_id = action.actionee_id.is
    val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        
    //val actionee_shield_in_front = CardPool.in_front_shield(actionee, gameo.card_list)
    val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
    
    val new_phase = RoomPhase.createFrom(roomphase) //.roomround_id(gameo.roomround.id.is)
                             .card_enum(card.to_card.card_enum.toString) //.power(roomphase.power.is)
                             .phase_type(RoomPhaseEnum.MBOLT_REACTION.toString)
                             .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
                             //.last_phase_type(roomphase.phase_type.is).last_phase_subtype(last)
    RoomPhase.push(gameo, new_phase)
    //gameo.refresh_roomphase
    
    /*
    
    if ((ActionReattack.enabled(gameo, actionee) &&
         (!ActionReattack.targetable_users(gameo, actionee).isEmpty) &&
         (!ActionReattack.targetable_cards(gameo, actionee).isEmpty)) || 
        (!ActionLight.targetable_cards(gameo, actionee).isEmpty)) {
      // 目標迎戰
      println("Process Attack - Reattack")
      val new_phase = RoomPhase.createFrom(roomphase) //.roomround_id(gameo.roomround.id.is)
                               .card_enum(card.to_card.card_enum.toString) //.power(roomphase.power.is)
                               .phase_type(RoomPhaseEnum.ATTACK_REACTION.toString).actioner_id(actionee.id.is).actionee_id(actioner.id.is)
                               //.deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is)).phase_flags(RoomPhaseEnum.REATTACK.toString)
      gameo.push_phase(new_phase)
      //gameo.refresh_roomphase
    } else if (actionee_shield_in_front.length != 0){
      // 目標以聖盾抵擋
      println("Process Attack - Shield")
      val shield_card = actionee_shield_in_front(0)
      shield_card.discard
          
      val talk = Talk.create.roomround_id(gameo.roomround.id.is).mtype(MTypeEnum.ACTION_SHIELD.toString)
                     .actioner_id(actionee_id)
      talk.save
      talk.send(room)
          
    } else if (actionee.heals.is != 0) {
      // 目標選擇治療與否
      println("Process Attack - Heal")
      val new_phase = RoomPhase.createFrom(roomphase) //.roomround_id(gameo.roomround.id.is)
                               .card_enum(card.to_card.card_enum.toString) //.power(roomphase.power.is)
                               .phase_type(RoomPhaseEnum.HEAL_REACTION.toString)
                               .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
                               //.deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is)).phase_flags(roomphase.phase_flags.is)
      gameo.phase_phase(new_phase)
      //gameo.refresh_roomphase
    } else {
      println("Process Attack - Damage")
      process_attack_damage(gameo, actioner, actionee)
    }
    
    */
  }
  
  /*
  def process_magic_damage(gameo : GameObject, actioner : UserEntry, actionee : UserEntry) = {
    //val actioner_id   = action.actioner_id.is
    //val actioner : UserEntry = UserEntry.get(actioner_id, gameo.userentrys)
    //val action_enum   = MTypeEnum.get_action(action.mtype.is)
    val userentrys    = gameo.userentrys
    val userentrys_rr = UserEntry.rr(userentrys)
    //val actionee_id = action.actionee_id.is
    //val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        
    val actionee_shield_in_front = CardPool.in_front_shield(actionee, gameo.card_list)
    val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)
    
    // 目標受傷害
    val message_type = MTypeEnum.RESULT_MAGIC
      
    val talk = Talk.create.roomround_id(gameo.roomround.id.is).mtype(message_type.toString)
                   .actioner_id(actionee.id.is).message_flags(roomphase.power.is.toString)
    talk.save
    talk.send(gameo.room)
      
    // 目標翻牌
    for (i <- 0 until roomphase.power.is) {
      val card = CardHelper.draw_card(gameo.room, gameo.card_list)
      card.owner_id(actionee.id.is).position(CardPositionEnum.HAND.toString).save
    }
    gameo.room.save
      
    val actionee_card_in_hand = CardPool.in_hand(actionee, gameo.card_list)
    if (actionee_card_in_hand.length > actionee.hand_max.is) {
      val new_phase = RoomPhase.create.roomround_id(gameo.roomround.id.is).card_enum(roomphase.card_enum.is).power(roomphase.power.is)
                               .phase_type(RoomPhaseEnum.DISCARD_REACTION.toString).player(actionee.id.is)
                               .deadline(PlummUtil.dateAddSecond(new java.util.Date(), gameo.room.reaction_time.is))
      RoomPhase.push_after(gameo.room, new_phase, gameo.roomphases)
    }
  } */
 
  def check_victory(gameo : GameObject) : Boolean = {
    if (gameo.room.status.is == RoomStatusEnum.ENDED)
      return true
    
    var result = false
    var team : UserEntryTeam  = null
    var is_win = false
    
    gameo.userentryteams.foreach { userentryteam =>
      if (userentryteam.moral.is <= 0 ) {
        if (userentryteam.has_team_flag(UserEntryTeamFlagEnum.MORALGUARD)) {
          userentryteam.moral(1).save
        } else {
          result = true
          team   = userentryteam
          is_win = false
        }
      } else if (userentryteam.grails.is >= 5) {
        result = true
        team   = userentryteam
        is_win = true
      }
    }
    
    if (result) {
      if (!is_win) 
        team = gameo.userentryteams.filterNot(_ == team)(0)
      
      val winning_roles = gameo.userentrys.filter(_.team_id.is == team.id.is).map(_.role.is).mkString(",")
      gameo.room.victory(team.team_type.is.toString).victory_all(winning_roles).save
    }
    
    result
  }
  
  /*
  def check_user_victory(userentry : UserEntry, victory_str : String) : Boolean = {
    val role = userentry.get_role
    
    if ((victory_str == RoomVictoryEnum.SHADOW_WIN.toString) && (role.role_side == RoleSideEnum.SHADOW)) 
      true
    else if ((victory_str == RoomVictoryEnum.HUNTER_WIN.toString) && (role.role_side == RoleSideEnum.HUNTER)) 
      true
    else if ((victory_str == RoomVictoryEnum.DUAL_WIN.toString) && (role.role_side != RoleSideEnum.NEUTRAL)) 
      true
    else if ((victory_str == RoomVictoryEnum.LOVER_WIN.toString) && (userentry.has_user_flag(UserEntryFlagEnum.LOVER)))
      true
    else if (userentry.has_user_flag(UserEntryFlagEnum.VICTORY))
      true
    else 
      false
  }
  */
  
  def process_victory(gameo : GameObject) = {
    gameo.room.stack_index(-1).processing(false)
    
    val new_roomround = RoomRound.create.room_id(gameo.room.id.is).round_no(gameo.roomround.round_no.is + 1)
                                        .last_round(gameo.roomround.id.is)
    new_roomround.save
    gameo.roomround = new_roomround
    val talk = Talk.create.roomround_id(new_roomround.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                   .message("遊戲結束 "+ (new java.util.Date).toString)
    talk.save
    
    val new_phase = RoomPhase.create.roomround_id(new_roomround.id.is).phase_no(0)
                                     .phase_type(RoomPhaseEnum.ENDED.toString)
    gameo.roomphases = List()
    gameo.push_phase(new_phase)
    //new_phase.save
    //val new_phases = RoomPhase.findAllByRoomRoundId(new_roomround.id.is)
    
    //val userentrys_v = userentrys.filter(x => (!x.revoked.is) && (x.has_user_flag(UserEntryFlagEnum.VICTORY)))
    //val userentrys_v_role = userentrys_v.map(_.get_real_role.role_enum.toString)
    
    gameo.room.status(RoomStatusEnum.ENDED.toString)
    gameo.room.save

    RoomActor.sendRoomMessage(gameo.room.id.is, SessionVarSet(room = gameo.room, roomround = gameo.roomround, roomphases = gameo.roomphases))
    RoomActor.sendRoomMessage(gameo.room.id.is, RoomForceUpdate(gameo.room.id.is ,List(ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.TEAM_TABLE, ForceUpdateEnum.CARD_TABLE,
                                                                                       ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.ACTION_BAR, ForceUpdateEnum.TALK_TABLE)))
  }
  
  def abandon(room : Room) = {
    val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is), OrderBy(RoomRound.round_no, Descending)).get
    val new_roomround = RoomRound.create.room_id(room.id.is).round_no(roomround.round_no.is + 1)
                                        .last_round(roomround.id.is)
    new_roomround.save
    val talk = Talk.create.roomround_id(new_roomround.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                   .message("遊戲結束 "+ (new java.util.Date).toString)
    talk.save
     
    val new_phase = RoomPhase.create.roomround_id(new_roomround.id.is).phase_no(0)
                            .phase_type(RoomPhaseEnum.ENDED.toString)
    new_phase.save
    val new_phases = RoomPhase.findAllByRoomRoundId(new_roomround.id.is)
             
    room.status(RoomStatusEnum.ENDED.toString).victory(RoomVictoryEnum.ABANDONED.toString)
    room.save
            
    RoomActor ! SessionVarSet(room = room, roomround = new_roomround, roomphases = new_phases)
    //RoomActor.sendRoomMessage(room_id, RoomForceUpdate(room_id ,List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.ACTION_BAR)))
    RoomActor.sendRoomMessage(room.id.is, RoomForceOut(room.id.is))
  }
}
