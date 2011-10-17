package org.plummtw.astgrail.model

import net.liftweb._
import net.liftweb.mapper._
import net.liftweb.util.FieldError

import scala.util.matching.Regex

import org.plummtw.astgrail.enum._
import org.plummtw.astgrail.card._
import org.plummtw.astgrail.data._
import org.plummtw.astgrail.util.PlummUtil
import org.plummtw.astgrail.heavy.GameProcessor

import net.liftweb.http.SessionVar

//object CardPool_R extends SessionVar[List[CardPool]](List())

class CardPool extends LongKeyedMapper[CardPool] with CreatedUpdated with IdPK {
  //override def dbName = "User"
  def getSingleton = CardPool
  
  object room_id       extends MappedLongForeignKey(this, Room)
  //object field_id      extends MappedLongForeignKey(this, Field)
  object card_no       extends MappedInt(this)
  //object card_type     extends MappedString(this, 1)
  object card          extends MappedString(this, 4)
  object owner_id      extends MappedLongForeignKey(this, UserEntry)
  object target_id      extends MappedLongForeignKey(this, UserEntry)
  
  object position      extends MappedString(this, 2)
  object flags         extends MappedString(this, 20)
  
  def discard(gameo : GameObject) = {
    val owner = UserEntry.get(owner_id.is, gameo.userentrys)
    val old_position = position.is
    owner_id(0).position(CardPositionEnum.DISCARD.toString).save
    if ((old_position == CardPositionEnum.HAND.toString) &&
        (owner.get_role == RoleMiko) & (owner.tapped.is)) {
      if (CardPool.in_hand(owner, gameo.card_list).length < 3)
        GameProcessor.check_miko_untap(gameo, owner)
    }
  }
  
  def to_front(gameo : GameObject, actioner_id : Long, actionee_id : Long) = {
    val owner = UserEntry.get(owner_id.is, gameo.userentrys)
    val old_position = position.is
    owner_id(actioner_id).target_id(actionee_id).position(CardPositionEnum.FRONT.toString).save
    if ((old_position == CardPositionEnum.HAND.toString) &&
        (owner.get_role == RoleMiko) & (owner.tapped.is)) {
      if (CardPool.in_hand(owner, gameo.card_list).length < 3)
        GameProcessor.check_miko_untap(gameo, owner)
    }
  }
  
  def to_card : Card = CardEnum.get_card(card.is.toString)
}

object CardPool extends CardPool with LongKeyedMetaMapper[CardPool] {
  override def fieldOrder = List(id, card_no, card, owner_id, target_id,
                              position, flags)

  def findAllByRoomId(room_id : Long) =
    CardPool.findAll(By(CardPool.room_id, room_id),
                     OrderBy(CardPool.card_no, Ascending))
  
  def getById(card_id : Long, card_list : List[CardPool]) =
    card_list.find(_.id.is == card_id).get
  
  def getByNo(card_no : Int, card_list : List[CardPool]) =
    card_list.find(_.card_no.is == card_no).get
  
  def in_hand(userentry : UserEntry, card_list : List[CardPool]) =
    card_list.filter(x => (x.owner_id.is == userentry.id.is) && 
                          (x.position.is == CardPositionEnum.HAND.toString))
       
  def in_hand_light(userentry : UserEntry, card_list : List[CardPool]) =
    card_list.filter(x => (x.owner_id.is == userentry.id.is) && 
                          (x.position.is == CardPositionEnum.HAND.toString) &&
                          (x.to_card.cardmagic_enum == CardMagicEnum.LIGHT))
       
  def discarded(userentry : UserEntry, card_list : List[CardPool]) =
    card_list.filter(x => (x.position.is == CardPositionEnum.DISCARD.toString))
  
  def in_front(userentry : UserEntry, card_list : List[CardPool]) =
    card_list.filter(x => (x.target_id.is == userentry.id.is) && 
                          (x.position.is == CardPositionEnum.FRONT.toString))
       
  def in_front_shield(userentry : UserEntry, card_list : List[CardPool]) =
    card_list.filter{x => val carddata = x.to_card
                          (x.target_id.is == userentry.id.is) && 
                          (x.position.is == CardPositionEnum.FRONT.toString) &&
                          ((carddata.cardmagic_enum == CardMagicEnum.SHIELD) ||
                           (carddata.has_action(MTypeEnum.ACTION_ANGEL_ANGELICWALL)))}
  
  def in_back(userentry : UserEntry, card_list : List[CardPool]) =
    card_list.filter(x => (x.target_id.is == userentry.id.is) && 
                          (x.position.is == CardPositionEnum.BACK.toString))
  
}

