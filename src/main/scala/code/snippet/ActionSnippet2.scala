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

class ActionSnippet2 extends Logger {
  
  def holybless(in: NodeSeq) = {
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
    
    def process : JsCmd = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      //val target_id : Long = try {
      //  target_str.toLong 
      //} catch {case e: Exception => 0}
      
      val cards = card_choose_list.map(CardPool.getByNo(_, gameo.card_list))
      
      if (card_choose_list.length < 2) {
        return Unblock & Alert("選擇卡片至少須 2 張")
      }
      
      //val card_attr = cards(0).to_card.cardattr_enum
      //cards.foreach { card1 =>
      //  val card_attr1 = card1.to_card.cardattr_enum
      //  if (card_attr1 != card_attr)
      //    return Unblock & Alert("選擇卡片須同屬性")
     // }
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_BISHOP_HOLYBLESS.toString)
                               .actioner_id(currentuserentry.id.is) //.actionee_id(target_id)
                               //.action_card(card.id.is).action_flags(card.card.is)
                               .action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                               .action_cards(card_choose_list.map(_.toString).mkString(","))
                               //.action_flags2(heal_str)
      
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select"  -> card_choose,
         //"user_select" -> SHtml.select(attack_targets.map(userentry1 => (userentry1.id.is.toString, userentry1.handle_name.is)), 
         //                              Full(attack_targets.head.id.is.toString), x => target_str = x),
         //"heal_select" -> SHtml.select(heal_map,
         //                       Full(heal_str),  x => heal_str = x),
         "holybless" -> ajaxSubmit("確定", () => process),
         "cancel"     -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def holywater(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    //val discard_number = cards_in_hand.length - currentuserentry.get_hand_max
    
    val attack_cards  = ActionBishopHolyWater.targetable_cards(gameo, currentuserentry)        
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    var card_str : String = card_map.head._1
    
    val cards_in_hand = CardPool.in_hand(currentuserentry, gameo.card_list)
    val card_map2  = cards_in_hand.map(x => (x.card_no.is.toString, x.to_card.card_name))
    
    var card_str2 : String = card_map2.head._1
    //val attack_targets = ActionSaintLanceRetribution.targetable_users(gameo, currentuserentry)
    //var target_str : String = ""
    //
    val attack_targets = ActionBishopHolyWater.targetable_users(gameo, currentuserentry)
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
      
      val card_no2 : Int = try {
        card_str2.toInt
      } catch {case e: Exception => 0}
      
      val card2 = CardPool.getByNo(card_no2, gameo.card_list)
      
      if (card_no == card_no2) {
        return Unblock & Alert("丟棄和給予不可同一張牌")
      }
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_BISHOP_HOLYWATER.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is)
                               .action_card2(card2.id.is)
                               .action_flags2(card2.card.is)
      
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select" -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "user_select" -> SHtml.select(attack_targets.map(userentry1 => (userentry1.id.is.toString, userentry1.handle_name.is)), 
                                       Full(attack_targets.head.id.is.toString), x => target_str = x),
         "card_select2" ->  SHtml.select(card_map2,
                                Full(card_str2),  x => card_str2 = x),
         "holywater" -> ajaxSubmit("確定", () => process),
         "cancel"     -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def holycontract(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    var heal_max = currentuserentry.heals.is
    
    var heal_map = Range(1, heal_max+1).reverse.map(x => (x.toString, x.toString))
      
    var heal_str : String = heal_map.head._1
      
    val attack_targets = ActionBishopHolyContract.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    def process : JsCmd = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_BISHOP_HOLYCONTRACT.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               //.action_card(card.id.is).action_flags(card.card.is)
                               .action_flags(heal_str)
                               //.action_cards(card_choose_list.map(_.toString).mkString(","))
                               //.action_flags2(heal_str)
      
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "heal_select" -> SHtml.select(heal_map,
                                Full(heal_str),  x => heal_str = x),
         "holycontract" -> ajaxSubmit("確定", () => process),
         "cancel"     -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def holyfield(in: NodeSeq) = {
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
    
    val attack_targets = ActionBishopHolyField.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    var option_map = Seq(("1", "法術傷害"), ("2", "增加治癒"))
      
    var option_str : String = option_map.head._1
                  
    def process : JsCmd = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val actionee = UserEntry.get(target_id, gameo.userentrys)
      
      val cards = card_choose_list.map(CardPool.getByNo(_, gameo.card_list))
      
      if (cards.length != 2) {
        return Unblock & Alert("選擇卡片數不為 2")
      }
      
      if ((option_str=="2") && (currentuserentry.team_id.is != actionee.team_id.is))
        return Unblock & Alert("增加治癒只能對隊友使用")
      
      if ((option_str=="1") && (currentuserentry.heals.is == 0))
        return Unblock & Alert("法術傷害時至少要有 1 點治療")
                    
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_BISHOP_HOLYFIELD.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               //.action_card(card.id.is).action_flags(card.card.is)
                               .action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                               .action_cards(card_choose_list.map(_.toString).mkString(","))
                               .action_flags2(option_str)
      
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> card_choose,
         "user_select"       -> SHtml.select(attack_targets.map(userentry1 => (userentry1.id.is.toString, userentry1.handle_name.is)), 
                                      Full(attack_targets.head.id.is.toString), x => target_str = x),
         "option_select"     -> SHtml.select(option_map,
                                      Full(option_str),  x => option_str = x),
         "holyfield" -> ajaxSubmit("確定", () => process),
         "cancel"     -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def reflect(in: NodeSeq) = {
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
    
    val attack_targets = ActionSageReflect.targetable_users(gameo, currentuserentry)
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
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_SAGE_REFLECT.toString)
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
         //"heal_select" -> SHtml.select(heal_map,
         //                       Full(heal_str),  x => heal_str = x),
         "reflect"    -> ajaxSubmit("確定", () => process),
         "cancel"     -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def magicbook(in: NodeSeq) = {
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
    
    val attack_targets = ActionSageMagicBook.targetable_users(gameo, currentuserentry)
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
      
      //val card_attrs = cards.map(_.to_card.cardattr_enum).toSet
      if (cards.length != cards.groupBy(_.to_card.cardattr_enum).toList.length)
        return Unblock & Alert("選擇卡片須不同屬性")
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_SAGE_MAGICBOOK.toString)
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
         //"heal_select" -> SHtml.select(heal_map,
         //                       Full(heal_str),  x => heal_str = x),
         "magicbook"  -> ajaxSubmit("確定", () => process),
         "cancel"     -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def holybook(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val cards_in_hand = CardPool.in_hand(currentuserentry, gameo.card_list)
    //val discard_number = cards_in_hand.length - currentuserentry.get_hand_max
    
    var card_choose_list : List[Int] = List()
    
    def card_choose = {
      <div align="center"><table> {
       for (card_group <- cards_in_hand.grouped(6)) yield
         <tr>{ for (card <- card_group) yield
           <td>{SHtml.checkbox(false, x => if (x) card_choose_list = card_choose_list ::: List(card.card_no.is))}{
               card.to_card.card_name.toString}</td> }</tr>
      } </table></div>
    }
    
    val attack_targets = ActionSageHolyBook.targetable_users(gameo, currentuserentry)
    var target_ids : List[Long] = List()
    
    def process : JsCmd = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      //val target_id : Long = try {
      //  target_str.toLong 
      //} catch {case e: Exception => 0}
      
      val cards = card_choose_list.map(CardPool.getByNo(_, gameo.card_list))
      
      if (card_choose_list.length < 3) {
        return Unblock & Alert("選擇卡片至少須 2 張")
      }
      
      //val card_attrs = cards.map(_.to_card.cardattr_enum).toSet
      if (cards.length != cards.groupBy(_.to_card.cardattr_enum).toList.length)
        return Unblock & Alert("選擇卡片須不同屬性")
      
      if (target_ids.length > cards.length - 2)
        return Unblock & Alert("選擇對象最多為卡片 - 2")
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_SAGE_HOLYBOOK.toString)
                               .actioner_id(currentuserentry.id.is) // .actionee_id(target_id)
                               //.action_card(card.id.is).action_flags(card.card.is)
                               .action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                               .action_cards(card_choose_list.map(_.toString).mkString(","))
                               .action_flags2(target_ids.map(_.toString).mkString(","))
      
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select"  -> card_choose,
         "user_select_table" -> UserEntryHelper.user_choose_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_ids = target_ids ::: List(x))),
         //"heal_select" -> SHtml.select(heal_map,
         //                       Full(heal_str),  x => heal_str = x),
         "holybook"   -> ajaxSubmit("確定", () => process),
         "cancel"     -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def thunderrune(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    val attack_cards  = ActionRuneMageThunderRune.targetable_cards(gameo, currentuserentry)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
    var card_str : String = card_map.head._1

    val back_cards  = CardPool.in_hand(currentuserentry, gameo.card_list)
    val card_map2  = 
      if (CardPool.in_back(currentuserentry, gameo.card_list).length >= RoleRuneMage.role_back_cards_max) Seq(("0", "不念咒"))
      else Seq(("0", "不念咒")) ++ back_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
    var card_str2 : String = card_map2.head._1
                                            
    val attack_targets = ActionRuneMageThunderRune.targetable_users(gameo, currentuserentry)
    var target_ids : List[Long] = List()
    
    var flag2_str  = ""
    
    def process : JsCmd = {
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)

      val card_no2 : Int = try {
        card_str2.toInt
      } catch {case e: Exception => 0}
      
      //val card2 = CardPool.getByNo(card_no2, gameo.card_list)
      
      if (card_no == card_no2)
        return Unblock & Alert("棄牌與蓋牌須不同")
      
      val card_num2  =
        if (card_no2 != 0)                                              
          CardPool.getByNo(card_no2, gameo.card_list).card.is
        else "0"
      
      if (target_ids.length != 2)
          return Unblock & Alert("須指定 2 名角色")                                        
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_RUNEMAGE_THUNDERRUNE.toString)
                               .actioner_id(currentuserentry.id.is) // .actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is + "," + card_num2)
                               .action_card2(card_no2)
                               //.action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                               //.action_cards(card_choose_list.map(_.toString).mkString(","))
                               .action_flags2(target_ids.map(_.toString).mkString(","))
      
       if (flag2_str != "")
         action.action_flags(action.action_flags.is + "," + flag2_str)
      
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_choose_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_ids = target_ids ::: List(x))),
         "back_card"         -> SHtml.select(card_map2,
                                Full(card_str2),  x => card_str2 = x),
         "special"           -> (if (currentuserentry.gems.is + currentuserentry.crystals.is > 0)
                                   <span>靈力崩解{SHtml.checkbox(false, (if (_) flag2_str = MTypeEnum.ACTION_RUNEMAGE_USERUNE.toString))}</span>
                                 else <span></span>),
         //"heal_select" -> SHtml.select(heal_map,
         //                       Full(heal_str),  x => heal_str = x),
         "thunderrune"  -> ajaxSubmit("確定", () => process),
         "cancel"       -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }                                        
  
  def airrune(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    val attack_cards  = ActionRuneMageAirRune.targetable_cards(gameo, currentuserentry)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
    var card_str : String = card_map.head._1

    val back_cards  = CardPool.in_hand(currentuserentry, gameo.card_list)
    val card_map2  = 
      if (CardPool.in_back(currentuserentry, gameo.card_list).length >= RoleRuneMage.role_back_cards_max) Seq(("0", "不念咒"))
      else Seq(("0", "不念咒")) ++ back_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
    var card_str2 : String = card_map2.head._1
                                            
    val attack_targets = ActionRuneMageAirRune.targetable_users(gameo, currentuserentry)
    var target_ids : List[Long] = List()
    
    def process : JsCmd = {
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)

      val card_no2 : Int = try {
        card_str2.toInt
      } catch {case e: Exception => 0}
      
     if (card_no == card_no2)
        return Unblock & Alert("棄牌與蓋牌須不同")                                         
      
      val card_num2  =
        if (card_no2 != 0)                                              
          CardPool.getByNo(card_no2, gameo.card_list).card.is
        else "0"
      
      if (target_ids.length != 2)
          return Unblock & Alert("須指定 2 名角色")                                        
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_RUNEMAGE_AIRRUNE.toString)
                               .actioner_id(currentuserentry.id.is) // .actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is + "," + card_num2)
                               .action_card2(card_no2)
                               //.action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                               //.action_cards(card_choose_list.map(_.toString).mkString(","))
                               .action_flags2(target_ids.map(_.toString).mkString(","))
      
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_choose_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_ids = target_ids ::: List(x))),
         "back_card"         -> SHtml.select(card_map2,
                                Full(card_str2),  x => card_str2 = x),
         //"heal_select" -> SHtml.select(heal_map,
         //                       Full(heal_str),  x => heal_str = x),
         "airrune"      -> ajaxSubmit("確定", () => process),
         "cancel"       -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def airrune_discard(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_cards  = ActionRuneMageAirRuneDiscard.targetable_cards(gameo, currentuserentry)
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
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_RUNEMAGE_AIRRUNE_DISCARD.toString)
                               .actioner_id(currentuserentry.id.is) //.actionee_id(target_id)
                               .action_card2(card.id.is).action_flags(card.card.is)
                               //.action_flags2(target_ids.map(_.toString).mkString(","))
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         //"user_select_table" -> UserEntryHelper.user_choose_table(UserEntry.rr(gameo.userentrys), 
         //                       attack_targets, x => (target_ids = target_ids ::: List(x))),
         "airrune_discard"-> ajaxSubmit("確定",  () => {process;  Unblock}),
         "cancel"       -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def ghosts100(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    val attack_cards  = CardPool.in_back(currentuserentry, gameo.card_list)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
    var card_str : String = card_map.head._1

    val attack_targets = ActionRuneMage100Ghosts.targetable_users(gameo, currentuserentry)
    var target_ids : List[Long] = List()
    
    var flag2_str  = ""
    
    def process : JsCmd = {
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)

      if (card.to_card.cardattr_enum == CardAttrEnum.FIRE) {
        if (target_ids.length != 2)
          return Unblock & Alert("火系牌須指定 2 名角色")
      } else {
        if (target_ids.length != 1)
          return Unblock & Alert("非火系牌須指定 1 名角色")
      }
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_RUNEMAGE_100GHOSTS.toString)
                               .actioner_id(currentuserentry.id.is) // .actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is)
                               //.action_card2(card_no2)
                               //.action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                               //.action_cards(card_choose_list.map(_.toString).mkString(","))
                               .action_flags2(target_ids.map(_.toString).mkString(","))
      
       if (flag2_str != "")
         action.action_flags(card.card.is + "," + flag2_str)
      
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_choose_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_ids = target_ids ::: List(x))),
         "special"           -> (if (currentuserentry.gems.is + currentuserentry.crystals.is > 0)
                                   <span>靈力崩解{SHtml.checkbox(false, (if (_) flag2_str = MTypeEnum.ACTION_RUNEMAGE_USERUNE.toString))}</span>
                                 else <span></span>),
         //"heal_select" -> SHtml.select(heal_map,
         //                       Full(heal_str),  x => heal_str = x),
         "ghosts100"        -> ajaxSubmit("確定", () => process),
         "cancel"       -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }                                        
  
  def powerbless(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_cards  = ActionPrayerPowerBless.targetable_cards(gameo, currentuserentry)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    var card_str : String = card_map.head._1
      
    val attack_targets = ActionPrayerPowerBless.targetable_users(gameo, currentuserentry)
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
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_PRAYER_POWERBLESS.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is)
       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "powerbless" -> ajaxSubmit("確定", () => { process }),
         "cancel"     -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def fastbless(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_cards  = ActionPrayerFastBless.targetable_cards(gameo, currentuserentry)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    var card_str : String = card_map.head._1
      
    val attack_targets = ActionPrayerFastBless.targetable_users(gameo, currentuserentry)
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
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_PRAYER_FASTBLESS.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is)
       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "fastbless" -> ajaxSubmit("確定", () => { process }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def shinebelieve(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val cards_in_hand = CardPool.in_hand(currentuserentry, gameo.card_list)
    val discard_number = math.min(2, cards_in_hand.length) 
    
    var card_choose_list : List[Int] = List()
    
    def card_choose = {
      <div align="center"><table> {
       for (card_group <- cards_in_hand.grouped(4)) yield
         <tr>{ for (card <- card_group) yield
           <td>{SHtml.checkbox(false, x => if (x) card_choose_list = card_choose_list ::: List(card.card_no.is))}{
               card.to_card.card_name.toString}</td> }</tr>
      } </table></div>
    }
      
    val attack_targets = ActionPrayerShineBelieve.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    def process : JsCmd = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val cards = card_choose_list.map(CardPool.getByNo(_, gameo.card_list))
      
      if (card_choose_list.length != discard_number) {
        return Unblock & Alert("選擇卡片須為 " + discard_number + " 張")
      }
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_PRAYER_SHINEBELIEVE.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               //.action_card(card.id.is).action_flags(card.card.is)
                               .action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                               .action_cards(card_choose_list.map(_.toString).mkString(","))
       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select" -> card_choose,
         "user_select" -> SHtml.select(attack_targets.map(userentry1 => (userentry1.id.is.toString, userentry1.handle_name.is)), 
                                       Full(attack_targets.head.id.is.toString), x => target_str = x),
         "shinebelieve" -> ajaxSubmit("確定", () => { process }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def darkbelieve(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_targets = ActionPrayerDarkBelieve.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    def process : JsCmd = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_PRAYER_DARKBELIEVE.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               //.action_card(card.id.is).action_flags(card.card.is)
       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "darkbelieve" -> ajaxSubmit("確定", () => { process }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def swordki(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_targets = ActionSwordEmpSwordKi.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    var swordki_max = math.min(3, currentuserentry.yellow_index.is)
    
    var swordki_map = Range(1, swordki_max+1).reverse.map(x => (x.toString, x.toString))
      
    var swordki_str : String = swordki_map.head._1  
    
    def process : JsCmd = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_SWORDEMP_SWORDKI.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               //.action_card(card.id.is).action_flags(card.card.is)
                               .action_flags(swordki_str)
       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "swordki_select"    -> SHtml.select(swordki_map,
                                 Full(swordki_str),  x => swordki_str = x),
         "swordki"   -> ajaxSubmit("確定", () => { process }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def kishot(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_targets = ActionMonkKiShot.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    def process : JsCmd = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_MONK_KISHOT.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               //.action_card(card.id.is).action_flags(card.card.is)
                               //.action_flags(swordki_str)
       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "kishot"    -> ajaxSubmit("確定", () => { process }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def dragons100(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_targets = ActionMonk100Dragons.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    def process : JsCmd = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_MONK_100DRAGONS.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               //.action_card(card.id.is).action_flags(card.card.is)
                               //.action_flags(swordki_str)
       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "dragons100" -> ajaxSubmit("確定", () => { process }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def monkgod(in: NodeSeq) = {
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
    
    def process : JsCmd = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      //val target_id : Long = try {
      //  target_str.toLong 
      //} catch {case e: Exception => 0}
      
      val cards = card_choose_list.map(CardPool.getByNo(_, gameo.card_list))
      
      if (cards_in_hand.length - card_choose_list.length != 3) {
        return Unblock & Alert("剩下卡片須為 3 張")
      }
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_MONK_MONKGOD.toString)
                               .actioner_id(currentuserentry.id.is) //.actionee_id(target_id)
                               .action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                               .action_cards(card_choose_list.map(_.toString).mkString(","))
       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select"  -> card_choose,
         "monkgod" -> ajaxSubmit("確定", () => { process }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def taunt(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_targets = ActionBraveTaunt.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    def process : JsCmd = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_BRAVE_TAUNT.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               //.action_card(card.id.is).action_flags(card.card.is)
                               //.action_flags(swordki_str)
       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "taunt" -> ajaxSubmit("確定", () => { process }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def bloodpray(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val targets = gameo.userentrys.filter(x => (!x.revoked.is) && (x.team_id.is == currentuserentry.team_id.is) &&
                                               (x.id.is != currentuserentry.id.is))
    var target_str1 : String = ""
    var target_str2 : String = ""
    
    var heal_map = Range(0, currentuserentry.heals.is +1).reverse.map(x => (x.toString, x.toString))
      
    var heal_str1 : String = heal_map.head._1
    var heal_str2 : String = heal_map.head._1
    
    def process : JsCmd = {
      val heal1 = PlummUtil.parseLong(heal_str1)
      val heal2 = PlummUtil.parseLong(heal_str2)
      
      if (heal1 + heal2 == 0)
        return Unblock & Alert("治療數不可為 0")
      if (heal1 + heal2 > currentuserentry.heals.is)
        return Unblock & Alert("治療數超過角色治療數")
                                                              
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_REDKNIGHT_BLOODPRAY.toString)
                               .actioner_id(currentuserentry.id.is) //.actionee_id(target_id)
                               //.action_card(card.id.is).action_flags(card.card.is)
                               .action_flags(target_str1 + "," + target_str2)
                               .action_flags2(heal1 + "," + heal2)
       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "user_select1" -> SHtml.select(targets.map(userentry1 => (userentry1.id.is.toString, userentry1.handle_name.is)), 
                                       Full(targets.head.id.is.toString), x => target_str1 = x),
         "user_select2" -> SHtml.select(targets.map(userentry1 => (userentry1.id.is.toString, userentry1.handle_name.is)), 
                                       Full(targets.head.id.is.toString), x => target_str2 = x),
         "heal_select1" -> SHtml.select(heal_map,
                                Full(heal_str1),  x => heal_str1 = x),
         "heal_select2" -> SHtml.select(heal_map,
                                Full(heal_str2),  x => heal_str2 = x),
         "bloodpray" -> ajaxSubmit("確定", () => { process }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def bloodcross(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_targets = ActionRedKnightBloodCross.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    val cards_in_hand = CardPool.in_hand(currentuserentry, gameo.card_list).filter(x =>
      (x.to_card.cardtype_enum == CardTypeEnum.MAGIC))
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
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_REDKNIGHT_BLOODCROSS.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               .action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                               .action_cards(card_choose_list.map(_.toString).mkString(","))
                               //.action_card(card.id.is).action_flags(card.card.is)
                               //.action_flags(swordki_str)
       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> card_choose, 
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "bloodcross" -> ajaxSubmit("確定", () => { process }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def soulburst(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_cards  = ActionSoulMageSoulBurst.targetable_cards(gameo, currentuserentry)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    var card_str : String = card_map.head._1
      
    val attack_targets = ActionSoulMageSoulBurst.targetable_users(gameo, currentuserentry)
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
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_SOULMAGE_SOULBURST.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is)
       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "soulburst" -> ajaxSubmit("確定", () => { process }),
         "cancel"     -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def soulgive(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_cards  = ActionSoulMageSoulGive.targetable_cards(gameo, currentuserentry)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    var card_str : String = card_map.head._1
      
    val attack_targets = ActionSoulMageSoulGive.targetable_users(gameo, currentuserentry)
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
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_SOULMAGE_SOULGIVE.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is)
       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "soulgive" -> ajaxSubmit("確定", () => { process }),
         "cancel"     -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def soulsummon(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val cards_in_hand = CardPool.in_hand(currentuserentry, gameo.card_list)
                                .filter(_.to_card.cardtype_enum == CardTypeEnum.MAGIC)
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
      
    def process : JsCmd = {
      val cards = card_choose_list.map(CardPool.getByNo(_, gameo.card_list))
      
      if (card_choose_list.length == 0) {
        return Unblock & Alert("選擇卡片至少須 1 張")
      }
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_SOULMAGE_SOULSUMMON.toString)
                               .actioner_id(currentuserentry.id.is)// .actionee_id(target_id)
                               .action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                               .action_cards(card_choose_list.map(_.toString).mkString(","))

       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> card_choose,
         "soulsummon" -> ajaxSubmit("確定", () => { process }),
         "cancel"     -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def soulmirror(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val cards_in_hand = CardPool.in_hand(currentuserentry, gameo.card_list)
    val discard_number = math.min(cards_in_hand.length, 3)
    
    var card_choose_list : List[Int] = List()
    
    def card_choose = {
      <div align="center"><table> {
       for (card_group <- cards_in_hand.grouped(4)) yield
         <tr>{ for (card <- card_group) yield
           <td>{SHtml.checkbox(false, x => if (x) card_choose_list = card_choose_list ::: List(card.card_no.is))}{
               card.to_card.card_name.toString}</td> }</tr>
      } </table></div>
    }
     
    val attack_targets = ActionSoulMageSoulGive.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
                                                                                  
    def process : JsCmd = {
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
                                                                                    
      val cards = card_choose_list.map(CardPool.getByNo(_, gameo.card_list))
      
      if (card_choose_list.length != discard_number) {
        return Unblock & Alert("選擇卡片須 "+ discard_number +" 張")
      }
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_SOULMAGE_SOULMIRROR.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               .action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                               .action_cards(card_choose_list.map(_.toString).mkString(","))

       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> card_choose,
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "soulmirror" -> ajaxSubmit("確定", () => { process }),
         "cancel"     -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def soullink(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_targets = ActionSoulMageSoulLink.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
                                                                                  
    def process : JsCmd = {
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
                                                                                    
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_SOULMAGE_SOULLINK.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               //.action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                               //.action_cards(card_choose_list.map(_.toString).mkString(","))

       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "soullink"   -> ajaxSubmit("確定", () => { process }),
         "cancel"     -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def soullink_transfer(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val roomphase = RoomPhase.get_phase(gameo.room, gameo.roomphases)

    var heal_max =
      math.min(currentuserentry.blue_index.is, roomphase.power.is)
    
    var heal_map = Range(1, heal_max+1).reverse.map(x => (x.toString, x.toString))
      
    var heal_str : String = heal_map.head._1
      
    def process = {
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_SOULMAGE_SOULLINK_TRANSFER.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(currentuserentry.target_user.is)
                               .action_flags(heal_str)
                               .action_flags2(roomphase.phase_flags.is)
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    
    ajaxForm(bind("action", in,
         "power"             -> <span>{gameo.roomphase.power.is}</span>,
         "heal_select"  -> SHtml.select(heal_map,
                                Full(heal_str),  x => heal_str = x),
         "soullink_transfer"      -> ajaxSubmit("轉換", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def bloodcry(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_cards  = ActionMikoBloodCry.targetable_cards(gameo, currentuserentry)
    val card_map  = attack_cards.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    var card_str : String = card_map.head._1
      
    val attack_targets = ActionMikoBloodCry.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    var heal_map = Range(1, 4).reverse.map(x => (x.toString, x.toString))
    var heal_str : String = heal_map.head._1
    
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
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_MIKO_BLOODCRY.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               .action_card(card.id.is).action_flags(card.card.is)
                               .action_flags2(heal_str)
       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select"       -> SHtml.select(card_map,
                                Full(card_str),  x => card_str = x),
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "power"            -> SHtml.select(heal_map,
                                Full(heal_str),  x => heal_str = x),
         "bloodcry" -> ajaxSubmit("確定", () => { process }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def stickwith(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_targets = ActionMikoStickWith.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    
    def process : JsCmd = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_MIKO_STICKWITH.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               //.action_card(card.id.is).action_flags(card.card.is)
                               //.action_flags2(heal_str)
       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "stickwith" -> ajaxSubmit("確定", () => { process }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def bloodsorrow(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val attack_targets = ActionMikoBloodSorrow.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    var is_remove : Boolean = false
    
    
    def process : JsCmd = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_MIKO_BLOODSORROW.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               .action_flags((if (is_remove) "1" else ""))
                               //.action_card(card.id.is).action_flags(card.card.is)
                               //.action_flags2(heal_str)
       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(UserEntry.rr(gameo.userentrys), 
                                attack_targets, x => (target_str = x)),
         "bloodsorrow" -> ajaxSubmit("確定", () => { process }),
         "remove"      -> <span>移除{SHtml.checkbox(false, x => if (x) is_remove = true)}</span>,
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def reversebleed(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val cards_in_hand = CardPool.in_hand(currentuserentry, gameo.card_list)
    val discard_number = math.min(2, cards_in_hand.length)
    
    var card_choose_list : List[Int] = List()
    
    def card_choose = {
      <div align="center"><table> {
       for (card_group <- cards_in_hand.grouped(4)) yield
         <tr>{ for (card <- card_group) yield
           <td>{SHtml.checkbox(false, x => if (x) card_choose_list = card_choose_list ::: List(card.card_no.is))}{
               card.to_card.card_name.toString}</td> }</tr>
      } </table></div>
    }
      
    def process : JsCmd = {
      
      val cards = card_choose_list.map(CardPool.getByNo(_, gameo.card_list))
      
      if (card_choose_list.length != discard_number) {
        return Unblock & Alert("選擇卡片須為 " + discard_number + " 張")
      }
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_MIKO_REVERSEBLEED.toString)
                               .actioner_id(currentuserentry.id.is)//.actionee_id(target_id)
                               //.action_card(card.id.is).action_flags(card.card.is)
                               .action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                               .action_cards(card_choose_list.map(_.toString).mkString(","))
       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select" -> card_choose,
         "reversebleed" -> ajaxSubmit("確定", () => { process }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def bloodcurse(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val cards_in_hand = CardPool.in_hand(currentuserentry, gameo.card_list)
    val discard_number = math.min(3, cards_in_hand.length)
    
    var card_choose_list : List[Int] = List()
    
    def card_choose = {
      <div align="center"><table> {
       for (card_group <- cards_in_hand.grouped(4)) yield
         <tr>{ for (card <- card_group) yield
           <td>{SHtml.checkbox(false, x => if (x) card_choose_list = card_choose_list ::: List(card.card_no.is))}{
               card.to_card.card_name.toString}</td> }</tr>
      } </table></div>
    }
      
    val attack_targets = ActionMikoBloodCurse.targetable_users(gameo, currentuserentry)
    var target_str : String = ""
    
    def process : JsCmd = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val cards = card_choose_list.map(CardPool.getByNo(_, gameo.card_list))
      
      if (card_choose_list.length != discard_number) {
        return Unblock & Alert("選擇卡片須為 " + discard_number + " 張")
      }
      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_MIKO_BLOODCURSE.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               //.action_card(card.id.is).action_flags(card.card.is)
                               .action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                               .action_cards(card_choose_list.map(_.toString).mkString(","))
       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select" -> card_choose,
         "user_select" -> SHtml.select(attack_targets.map(userentry1 => (userentry1.id.is.toString, userentry1.handle_name.is)), 
                                       Full(attack_targets.head.id.is.toString), x => target_str = x),
         "bloodcurse" -> ajaxSubmit("確定", () => { process }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def dance(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val cards_in_hand = CardPool.in_hand(currentuserentry, gameo.card_list)
    val card_map  = Seq(("", "摸1張牌")) ++ cards_in_hand.map(x => (x.card_no.is.toString, x.to_card.card_name))
      
    var card_str : String = card_map.head._1
    
    def process : JsCmd = {
                                                                                                      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_BUTTERFLY_DANCE.toString)
                               .actioner_id(currentuserentry.id.is)//.actionee_id(target_id)
                               //.action_card(card.id.is).action_flags(card.card.is)
                               //.action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                               //.action_cards(card_choose_list.map(_.toString).mkString(","))
      if (card_str != "") {
        val card_no : Int = try {
          card_str.toInt
        } catch {case e: Exception => 0}
      
        val card = CardPool.getByNo(card_no, gameo.card_list)
        action.action_card(card.id.is).action_flags(card.card.is)
      }
       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select" -> SHtml.select(card_map, Full(card_str), x => card_str = x),
         "dance"       -> ajaxSubmit("確定", () => { process }),
         "cancel"      -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def pilgrimage(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val cards_in_back = CardPool.in_back(currentuserentry, gameo.card_list)
    val card_map  = cards_in_back.map(x => (x.card_no.is.toString, x.to_card.card_name))
    var card_str : String = card_map.head._1
    
    val wither_map = Seq(("0", "不展示")) ++ UserEntry.rr(gameo.userentrys).map(userentry1 => (userentry1.id.is.toString, userentry1.handle_name.is))
    var wither_str : String = wither_map.head._1
                                                                                                    
    def process : JsCmd = {
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      val card = CardPool.getByNo(card_no, gameo.card_list)
      
      if ((card.to_card.cardtype_enum != CardTypeEnum.MAGIC) &&
          (wither_str != "0"))
        return Unblock & Alert("非法術牌不得展示")
                                                                                                      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_BUTTERFLY_PILGRIMAGE.toString)
                               .actioner_id(currentuserentry.id.is)//.actionee_id(target_id)
                               //.action_card(card.id.is).action_flags(card.card.is)
                               //.action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                               .action_flags(card.to_card.card_enum.toString)
                               .action_cards(card_str)
                               .action_flags2(wither_str)

       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select" -> SHtml.select(card_map, Full(card_str), x => card_str = x),
         "wither_select" -> <span>凋零：{SHtml.select(wither_map, 
                                          Full(wither_str), x => wither_str = x)}</span>,
         "pilgrimage"  -> ajaxSubmit("確定", () => { process }),
         "cancel"      -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def poisonpowder(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val cards_in_back = CardPool.in_back(currentuserentry, gameo.card_list)
    val card_map  = cards_in_back.map(x => (x.card_no.is.toString, x.to_card.card_name))
    var card_str : String = card_map.head._1
    
    val wither_map = Seq(("0", "不展示")) ++ UserEntry.rr(gameo.userentrys).map(userentry1 => (userentry1.id.is.toString, userentry1.handle_name.is))
    var wither_str : String = wither_map.head._1
    
    def process : JsCmd = {
      val card_no : Int = try {
        card_str.toInt
      } catch {case e: Exception => 0}
      
      val card = CardPool.getByNo(card_no, gameo.card_list)
                                                                                                      
      if ((card.to_card.cardtype_enum != CardTypeEnum.MAGIC) &&
          (wither_str != "0"))
        return Unblock & Alert("非法術牌不得展示")
                                                                                                      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_BUTTERFLY_POISONPOWDER.toString)
                               .actioner_id(currentuserentry.id.is)//.actionee_id(target_id)
                               // .action_card(card.id.is).action_flags(card.card.is)
                               .action_flags(card.to_card.card_enum.toString)
                               .action_cards(card_str)
                               .action_flags2(wither_str)
       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select"   -> SHtml.select(card_map, Full(card_str), x => card_str = x),
         "wither_select" -> <span>凋零：{SHtml.select(wither_map, 
                                          Full(wither_str), x => wither_str = x)}</span>,
         "poisonpowder"  -> ajaxSubmit("確定", () => { process }),
         "cancel"      -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def mirrorflower(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    //println("DEBUG attack : " + currentuserentry.toString)
    
    val cards_in_back = CardPool.in_back(currentuserentry, gameo.card_list)
    val card_map  = cards_in_back.map(x => (x.card_no.is.toString, x.to_card.card_name))
    var card_str1 : String = card_map.head._1
    var card_str2 : String = card_map.head._1
    
    val wither_map = Seq(("0", "不展示")) ++ UserEntry.rr(gameo.userentrys).map(userentry1 => (userentry1.id.is.toString, userentry1.handle_name.is))
    var wither_str1 : String = wither_map.head._1
    var wither_str2 : String = wither_map.head._1
    
    def process : JsCmd = {
      val card_no1 : Int = try {
        card_str1.toInt
      } catch {case e: Exception => 0}
      val card1 = CardPool.getByNo(card_no1, gameo.card_list)
      val card1data = card1.to_card
      
      val card_no2 : Int = try {
        card_str2.toInt
      } catch {case e: Exception => 0}
      val card2 = CardPool.getByNo(card_no2, gameo.card_list)
      val card2data = card2.to_card
                                                                                                      
      if (card_no1 == card_no2)
        return Unblock & Alert("不得選擇同樣卡片")
      if ((card1data.cardtype_enum != CardTypeEnum.MAGIC) &&
          (wither_str1 != "0"))
        return Unblock & Alert("牌1 : 非法術牌不得展示")
      if ((card2data.cardtype_enum != CardTypeEnum.MAGIC) &&
          (wither_str2 != "0"))
        return Unblock & Alert("牌2 : 非法術牌不得展示")
      if (card1data.cardattr_enum != card2data.cardattr_enum)
        return Unblock & Alert("非同系卡片")
                                                                                                      
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_BUTTERFLY_MIRRORFLOWER.toString)
                               .actioner_id(currentuserentry.id.is)//.actionee_id(target_id)
                               // .action_card(card.id.is).action_flags(card.card.is)
                               .action_flags(card1.to_card.card_enum.toString + "," + card2.to_card.card_enum.toString)
                               .action_cards(card_str1 + "," + card_str2)
                               .action_flags2(wither_str1 + "," + wither_str2)
       
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select1"   -> SHtml.select(card_map, Full(card_str1), x => card_str1 = x),
         "card_select2"   -> SHtml.select(card_map, Full(card_str2), x => card_str2 = x),
         "wither_select1" -> <span>凋零：{SHtml.select(wither_map, 
                                          Full(wither_str1), x => wither_str1 = x)}</span>,
         "wither_select2" -> <span>凋零：{SHtml.select(wither_map, 
                                          Full(wither_str2), x => wither_str2 = x)}</span>,
         "mirrorflower"  -> ajaxSubmit("確定", () => { process }),
         "cancel"      -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def backdiscard(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val cards_in_back = CardPool.in_back(currentuserentry, gameo.card_list)
    val discard_number = math.max(0, cards_in_back.length - currentuserentry.get_role.role_back_cards_max)
    
    var card_choose_list : List[Int] = List()
    
    def card_choose = {
      <div align="center"><table> {
       for (card_group <- cards_in_back.grouped(4)) yield
         <tr>{ for (card <- card_group) yield
           <td>{SHtml.checkbox(false, x => if (x) card_choose_list = card_choose_list ::: List(card.card_no.is))}{
               card.to_card.card_name.toString}</td> }</tr>
      } </table></div>
    }
      
    def process = {
      val cards = card_choose_list.map(CardPool.getByNo(_, gameo.card_list))
                
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_BUTTERFLY_BACKDISCARD.toString)
                               .actioner_id(currentuserentry.id.is)
                               .action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                               .action_cards(card_choose_list.map(_.toString).mkString(","))
      
      RoomActor ! SignalAction(gameo.room, action)
    }
    
    
    ajaxForm(bind("action", in,
         "discard_num" -> <span>{discard_number}</span>,
         "card_select" -> card_choose,
         "backdiscard" -> ajaxSubmit("棄牌", () => { 
                          if (card_choose_list.length != discard_number) 
                            Unblock & Alert("你選擇了" + card_choose_list.length + "張，選擇數不為 " + discard_number.toString)
                          else {
                            process ; Unblock
                          }
                        }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def reversefly(in: NodeSeq) = {
    val gameo  = GameO_R.get
    val roomround = gameo.roomround
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    val cards_in_hand = CardPool.in_hand(currentuserentry, gameo.card_list)
    val discard_number = math.min(cards_in_hand.length, 2)
    
    var card_choose_list : List[Int] = List()
    
    def card_choose = {
      <div align="center"><table> {
       for (card_group <- cards_in_hand.grouped(6)) yield
         <tr>{ for (card <- card_group) yield
           <td>{SHtml.checkbox(false, x => if (x) card_choose_list = card_choose_list ::: List(card.card_no.is))}{
               card.to_card.card_name.toString}</td> }</tr>
      } </table></div>
    }
    
    val reversefly_map = Seq(("1", "法術傷害"), ("2", "棄繭移蛹"), ("3", "自傷移蛹"))
    var reversefly_str : String = reversefly_map.head._1
    
    val attack_targets = UserEntry.rr(gameo.userentrys)
    var target_str : String = ""
    
    val cards_in_back = CardPool.in_back(currentuserentry, gameo.card_list)
    val card_map  = Seq(("", "無")) ++ cards_in_back.map(x => (x.card_no.is.toString, x.to_card.card_name))
    var card_str1 : String = card_map.head._1
    var card_str2 : String = card_map.head._1
    
    val wither_map = Seq(("0", "不展示")) ++ UserEntry.rr(gameo.userentrys).map(userentry1 => (userentry1.id.is.toString, userentry1.handle_name.is))
    var wither_str1 : String = wither_map.head._1
    var wither_str2 : String = wither_map.head._1
    
    def process : JsCmd = {
      val cards = card_choose_list.map(CardPool.getByNo(_, gameo.card_list))
      if (card_choose_list.length != discard_number) {
        return Unblock & Alert("選擇卡片須為 " + discard_number + " 張")
      }
                                                                                                                    
      val action = 
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_BUTTERFLY_REVERSEFLY.toString)
                               .actioner_id(currentuserentry.id.is)//.actionee_id(target_id)
                               .action_flags(cards.map(_.to_card.card_enum.toString).mkString(","))
                               .action_cards(card_choose_list.map(_.toString).mkString(","))
      if (reversefly_str == "1") {
        val target_id : Long = try {
          target_str.toLong 
        } catch {case e: Exception => 0}
        action.actionee_id(target_id)
      } else if (reversefly_str == "2") {
        val card_no1 : Int = try {
          card_str1.toInt
        } catch {case e: Exception => 0}
        val card1 = CardPool.getByNo(card_no1, gameo.card_list)
      
        val card_no2 : Int = try {
          card_str2.toInt
        } catch {case e: Exception => 0}
        val card2 = CardPool.getByNo(card_no2, gameo.card_list)
      
        if (card_no1 == card_no2)
          return Unblock & Alert("不得選擇同樣卡片")
        if (card_no1 == 0)
          return Unblock & Alert("牌1 : 不得選擇無")
        if (card_no2 == 0)
          return Unblock & Alert("牌2 : 不得選擇無")
        if ((card1.to_card.cardtype_enum != CardTypeEnum.MAGIC) &&
            (wither_str1 != "0"))
          return Unblock & Alert("牌1 : 非法術牌不得展示")
        if ((card2.to_card.cardtype_enum != CardTypeEnum.MAGIC) &&
            (wither_str2 != "0"))
          return Unblock & Alert("牌2 : 非法術牌不得展示")

        action.action_flags(action.action_flags.is + "," + card1.to_card.card_enum.toString + "," + card2.to_card.card_enum.toString)
               .action_cards(action.action_cards.is + "," + card_str1 + "," + card_str2)
               .action_flags2(wither_str1 + "," + wither_str2)
      } else {
        
      }
                                                                                                            
      RoomActor ! SignalAction(gameo.room, action)
      Unblock
    }
    
    ajaxForm(bind("action", in,
         "card_select"    -> card_choose,
         "reversefly_select"   -> SHtml.select(reversefly_map, Full(reversefly_str), x => reversefly_str = x),
         "user_select" -> <span>法傷目標：{SHtml.select(attack_targets.map(userentry1 => (userentry1.id.is.toString, userentry1.handle_name.is)), 
                                          Full(target_str), x => target_str = x)}</span>,
         "card_select1"   -> SHtml.select(card_map, Full(card_str1), x => card_str1 = x),
         "card_select2"   -> SHtml.select(card_map, Full(card_str2), x => card_str2 = x),
         "wither_select1" -> <span>凋零：{SHtml.select(wither_map, 
                                          Full(wither_str1), x => wither_str1 = x)}</span>,
         "wither_select2" -> <span>凋零：{SHtml.select(wither_map, 
                                          Full(wither_str2), x => wither_str2 = x)}</span>,
         "reversefly"  -> ajaxSubmit("確定", () => { process }),
         "cancel"      -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
}
