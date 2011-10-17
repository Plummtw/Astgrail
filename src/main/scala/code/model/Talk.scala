package org.plummtw.astgrail.model 
 
import net.liftweb._ 
import mapper._ 
import http._ 
import SHtml._ 
import util._

import org.plummtw.astgrail.actor._

class Talk extends LongKeyedMapper[Talk]  with CreatedUpdated with IdPK {
  def getSingleton = Talk // what's the "meta" object
  //def primaryKeyField = id

  // the primary key
  // object id            extends MappedLongIndex(this)
  object roomround_id    extends MappedLongForeignKey(this, RoomRound) 
  object actioner_id       extends MappedLongForeignKey(this, UserEntry)
  object actionee_id      extends MappedLongForeignKey(this, UserEntry)
  object cssclass         extends MappedString(this, 20)
  
  object message        extends MappedString(this, 600)
  object mtype          extends MappedString(this, 4)
  
  object message_flags   extends MappedString(this, 200)
  object message_flags2    extends MappedString(this, 200)
  
  def send(room : Room) =
    RoomActor.sendRoomMessage(room.id.is, NewMessage(room, this))
}

object Talk extends Talk with LongKeyedMetaMapper[Talk] {
  override def fieldOrder = List(id, roomround_id, actioner_id, actionee_id, cssclass, message, 
                              mtype, message_flags, message_flags2)
}