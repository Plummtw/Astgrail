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
import org.plummtw.astgrail.snippet._
import org.plummtw.astgrail.data._
import org.plummtw.astgrail.card._
//import org.plummtw.astgrail.util.PlummUtil

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer 

object CardHelper {
  val random = scala.util.Random
  
  def shuffle_cardpool(room : Room, card_pool : List[CardPool]) {
    //val room = Room_R.get
    //val card_pool = CardPool.findAll(By(CardPool.room_id, room.id.is),
    //                              By(CardPool.card_type, card_type.toString))
    
    if (card_pool.length == 0) {
      val card_list = CardEnum.CARD_LIST
      
      val java_card_list: java.util.List[CardEnum.Value] = ListBuffer(card_list: _*)
      
      java.util.Collections.shuffle(java_card_list)
      
      val card_list2 = java_card_list.toList
      var card_no = 1
      card_list2 foreach { card_data =>
        val card = CardPool.create.room_id(room.id.is).card_no(card_no)
                           .card(card_data.toString).position(CardPositionEnum.DECK.toString)
        card.save
        card_no = card_no + 1
      }
    } else {
      val java_card_no_list: java.util.List[Int] = new java.util.ArrayList()
      for (i <- 0 until card_pool.length) 
        java_card_no_list.add(i)  
      
      java.util.Collections.shuffle(java_card_no_list)
      
      var card_index = 0
      card_pool foreach { card =>
        if (card.position.is == CardPositionEnum.DISCARD.toString)
          card.position(CardPositionEnum.DECK.toString)
        card.card_no(java_card_no_list.get(card_index))
        card.save
        card_index = card_index + 1
      }
    }
  }
  
  def fake_shuffle_cardpool(room : Room, card_pool : List[CardPool]) {
    DB.use(DefaultConnectionIdentifier) { conn =>
      // , damaged='0'
      DB.prepareStatement("update CardPool set position = 'DE' where position = 'DI' and room_id = ?", conn) { stmt =>
        stmt.setLong(1, room.id.is)
        stmt.executeUpdate()
      }
     }
  }
  
  def draw_card(gameo : GameObject) : CardPool = {
    val room = gameo.room
    var card_list = gameo.card_list
         //CardPool.findAll(By(CardPool.room_id, room.id.is),
    //                                 OrderBy(CardPool.card_no, Ascending))
    //var card_pool = card_list.filter(x => x.card_type.is == card_type.toString)
    
    var card_index = room.card_index.is
    var card_pool  = card_list
    
    var result : CardPool = null
    do {
      if (card_index >= card_list.length) {
        fake_shuffle_cardpool(room, card_list)
        card_index = 0
      
        card_list  = CardPool.findAllByRoomId(room.id.is)
        gameo.card_list = card_list
      }
    
      result = card_list(card_index)
      card_index = card_index + 1
    } while ((result.position.is != CardPositionEnum.DECK.toString))
    
    room.card_index(card_index)
    //room.save
    
    //if (is_update) {
    //  RoomActor ! SessionVarSet(room = room, card_list = card_list)
    //  RoomActor ! RoomForceUpdate(room.id.is, List(ForceUpdateEnum.CARD_TABLE))
    //}
    
    result
  }
  
  def card_table(room : Room, currentuserentry : UserEntry, card_list : List[CardPool]) = {
    if ((room.status.is == RoomStatusEnum.WAITING.toString) || (card_list.length == 0))
      <span></span>
    else {
      val remains = card_list.filter(x => (x.card_no.is >= room.card_index.is) &&
                                          (x.position.is == CardPositionEnum.DECK.toString))
      
      val discards = card_list.filter(x => (x.position.is == CardPositionEnum.DISCARD.toString))
      
      //val hand_cards =
      //  if (currentuserentry == null) ""
      //  else "手牌 : " + CardPool.in_hand(currentuserentry, card_list).map(_.to_card.card_name).mkString(",")
      
      val hand_cards = 
        if (currentuserentry == null) List()
        else CardPool.in_hand(currentuserentry, card_list).map(_.to_card)
      
      def hand_card_seq(x : Card) =
        <span title={x.description}>{x.card_name}</span>
      
      val hand_cards_seq =
        if (hand_cards.isEmpty) Seq()
        else hand_cards.tail.foldLeft(Seq[Object](hand_card_seq(hand_cards.head)))((x,y)=> x ++ Seq(",", hand_card_seq(y)))
      
      <span>
        牌庫剩餘：{remains.length.toString}
        棄牌區：{discards.length.toString} {if (hand_cards.isEmpty) "" else "手牌："}{hand_cards_seq}
      </span>
    }
  }
  
  /*
  def draw_specific(room : Room, card_str : String) : CardPool = {
    
    val card_id = try {
      card_str.toLong 
    } catch { case e : Exception => 0L}
    
    //val room = Room_R.get
    val card_list = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                        OrderBy(CardPool.card_no, Ascending))
    //val card_pool = card_list.filter(x => x.card_type.is == card_type.toString)

    val result = card_list.filter(_.id.is == card_id)(0)
    
    if (!CardEnum.get_card(result.card.is).isInstanceOf[Equipment])
      result.discarded(true).save
    

    RoomActor ! SessionVarSet(room = room, card_list = card_list)
    RoomActor ! RoomForceUpdate(room.id.is, List(ForceUpdateEnum.CARD_TABLE))
    
    result
  }
  
  def peek_card2(room : Room, card_type : CardTypeEnum.Value) : (CardPool, CardPool) = {
    //val room = Room_R.get
    var card_list = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                     OrderBy(CardPool.card_no, Ascending))
    var card_pool = card_list.filter(x => x.card_type.is == card_type.toString)
    
    var card_index = card_type match {
      case CardTypeEnum.BLACK => room.blackcard_index.is
      case CardTypeEnum.WHITE => room.whitecard_index.is
      case CardTypeEnum.GREEN => room.greencard_index.is
    }
    
    var result : CardPool = null
    if (card_index >= card_pool.length) {
      shuffle_cardpool(room, card_type, card_pool)
      card_index = 0
      
      card_list = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                   OrderBy(CardPool.card_no, Ascending))
      card_pool = card_list.filter(x => x.card_type.is == card_type.toString)
    }
    
    var card_pool2 = card_pool.filter(x => (x.card_no.is >= card_index) &&
                                           (!x.discarded.is) && (x.owner_id.is == 0))
    
    if (card_pool2.length >= 2)
      ((card_pool2(0), card_pool2(1)))
    else if (card_pool2.length == 1)
      ((card_pool2(0), card_pool2(0)))
    else {
      shuffle_cardpool(room, card_type, card_pool)
      card_index = 0
      
      card_list = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                   OrderBy(CardPool.card_no, Ascending))
      card_pool = card_list.filter(x => x.card_type.is == card_type.toString)
      card_pool2 = card_pool.filter(x => (x.card_no.is >= card_index) &&
                                           (!x.discarded.is) && (x.owner_id.is == 0))
      card_type match {
        case CardTypeEnum.BLACK => room.blackcard_index(card_index)
        case CardTypeEnum.WHITE => room.whitecard_index(card_index)
        case CardTypeEnum.GREEN => room.greencard_index(card_index)
      }
    
      room.save
    
      RoomActor ! SessionVarSet(room = room, card_list = card_list)
      RoomActor ! RoomForceUpdate(room.id.is, List(ForceUpdateEnum.CARD_TABLE))

      ((card_pool2(0), card_pool2(1)))
    }
  }
  */
}
