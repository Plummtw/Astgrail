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

import org.plummtw.astgrail.enum._
//import org.plummtw.astgrail.util.PlummUtil

class RoomPhaseLog extends LongKeyedMapper[RoomPhaseLog] with CreatedUpdated with IdPK {
  def getSingleton = RoomPhaseLog // what's the "meta" object

  object roomround_id  extends MappedLongForeignKey(this, RoomRound)
  
  object phase_no       extends MappedInt(this)
  object phase_type     extends MappedString(this, 5)
  object phase_subtype  extends MappedString(this, 5)
  object actioner_id      extends MappedLongForeignKey(this, UserEntry) 
  object actionee_id      extends MappedLongForeignKey(this, UserEntry) 
  object actionee2_id     extends MappedLongForeignKey(this, UserEntry) 
  
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

object RoomPhaseLog extends RoomPhaseLog with LongKeyedMetaMapper[RoomPhaseLog] with Logger {
  override def fieldOrder = List(id, roomround_id, phase_no, phase_type, phase_subtype,
                                 actioner_id, actionee_id, actionee2_id, card_enum, action_card, action_cards,
                                 action_flags, action_flags2, stack, stage, power, deadline,
                                 last_phase_type, last_phase_subtype, phase_flags)
  
  def create_log(roomround : RoomRound, roomphase : RoomPhase) {
    // 未 copy id roomround_id phase_no stack
    val roomphaselog = RoomPhaseLog.create.roomround_id(roomround.id.is).phase_no(roomround.phaselog_no.is)
                .phase_type(roomphase.phase_type.is).phase_subtype(roomphase.phase_subtype.is)
                .actioner_id(roomphase.actioner_id.is).actionee_id(roomphase.actionee_id.is)
                .actionee2_id(roomphase.actionee2_id.is)
                .card_enum(roomphase.card_enum.is).action_card(roomphase.action_card.is).action_cards(roomphase.action_cards.is)
                .action_flags(roomphase.action_flags.is).action_flags2(roomphase.action_flags2.is)
                .stage(roomphase.stage.is).power(roomphase.power.is).deadline(roomphase.deadline.is)
                .last_phase_type(roomphase.last_phase_type.is).last_phase_subtype(roomphase.last_phase_subtype.is)
                .phase_flags(roomphase.phase_flags.is)
     roomphaselog.save
     roomround.phaselog_no(roomround.phaselog_no.is + 1).save
  }
}