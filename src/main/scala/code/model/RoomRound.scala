package org.plummtw.astgrail.model

import net.liftweb._
import net.liftweb.mapper._
//import net.liftweb.util.FieldError

import net.liftweb.http.RequestVar
import net.liftweb.http.SessionVar

//import scala.util.matching.Regex

import org.plummtw.astgrail.enum._
//import org.plummtw.astgrail.util.PlummUtil

//object RoomRound_E extends SessionVar[RoomRound](null)
//object RoomRound_R extends SessionVar[RoomRound](null)

class RoomRound extends LongKeyedMapper[RoomRound] with CreatedUpdated with IdPK {
  def getSingleton = RoomRound // what's the "meta" object
  //def primaryKeyField = id

  // the primary key
  // object id            extends MappedLongIndex(this)
  object room_id          extends MappedLongForeignKey(this, Room)
  object last_round        extends MappedLongForeignKey(this, RoomRound) 
  
  object round_no         extends MappedInt(this)
  object phaselog_no       extends MappedInt(this)
  object actioner_id        extends MappedLongForeignKey(this, UserEntry) 
  object additional         extends MappedInt(this)
  object additional_flag     extends MappedString(this, 1)
  
  object turn_no           extends MappedInt(this)
  object round_flags        extends MappedString(this, 20)
  
  //object reaction_player  extends MappedLongForeignKey(this, UserEntry) 
  //object reaction_type    extends MappedString(this,2)
  
  //object deadline       extends MappedDateTime(this)
  
  def has_flag(flag : RoomRoundFlagEnum.Value) =
    round_flags.is.indexOf(flag.toString) != -1
  
  def add_flag(flag : RoomRoundFlagEnum.Value) = {
    round_flags(round_flags.is.toString + flag.toString)
    this
  }
  
  def remove_flag(flag : RoomRoundFlagEnum.Value) = {
    round_flags(round_flags.is.toString.replace(flag.toString, ""))
    this
  }
  
}

object RoomRound extends RoomRound with LongKeyedMetaMapper[RoomRound] {
  override def fieldOrder = List(id, room_id, last_round, round_no,
                                  actioner_id, additional, additional_flag, 
                                  turn_no, round_flags)
}

