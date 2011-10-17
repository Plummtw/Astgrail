package org.plummtw.astgrail.actor

import _root_.net.liftweb._
import net.liftweb.mapper._
import http._
import SHtml._
import js._
import JsCmds._
import common._
import util._
import Helpers._
import actor._

//import scala.xml.NodeSeq

import collection.mutable.HashMap

import org.plummtw.astgrail.enum._
import org.plummtw.astgrail.model._
import org.plummtw.astgrail.util._
import org.plummtw.astgrail.comet._
import org.plummtw.astgrail.data._
import org.plummtw.astgrail.heavy._

/*
case class RoomSubscribe(comet : GameComet, room_id : Long, userentry_id : Long)
case class RoomUnsubscribe(comet : GameComet, room_id : Long, userentry_id : Long)
case class RoomDeleteRoom(room_id : Long)

case class NewMessage(room : Room, talk : Talk)
case class SignalAction(action : Action)
case class SignalAbandon(room : Room)
case class SignalDeadline(room : Room, roomround : RoomRound, roomphase : RoomPhase)

case class RoomForceOut(room_id : Long)
case class ForceOut(userentry_id : Long)
case class ForceLogOut(userentry_id : Long)

case class SessionVarSet(room : Room = null, roomround : RoomRound = null, //roomphase : RoomPhase = null, 
                         roomphases : List[RoomPhase] = List(), userentrys : List[UserEntry] = List(),
                         userentryteams : List[UserEntryTeam] = List(), card_list : List[CardPool] = List())
case class RoomForceUpdate(room_id : Long, updates : List[ForceUpdateEnum.Value])
case class UserEntryForceUpdate(userentry_id : Long, updates : List[ForceUpdateEnum.Value])
*/

//case class NewRoomRound(room_id : Long, roomround : RoomRound)

class SingleRoomActor extends LiftActor with Logger {
 
  override def messageHandler = (in:Any) => in match {
    case SignalAction(room, action) =>
      RoomActor.process_signal_action(action)
  }
}
