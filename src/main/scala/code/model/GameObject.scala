package org.plummtw.astgrail.model

import net.liftweb.http.SessionVar
import org.plummtw.astgrail.enum._

object GameO_E      extends SessionVar[GameObject](null)
object GameO_R      extends SessionVar[GameObject](null)

class GameObject(
  var room : Room, var roomround : RoomRound, //var roomphase : RoomPhase,
  var roomphases : List[RoomPhase], //var currentuserentry : UserEntry,
  var userentrys : List[UserEntry], 
  var userentryteams : List[UserEntryTeam],
  var card_list : List[CardPool]) {
  
  var update_list : List[ForceUpdateEnum.Value] = List()
  
  def add_update(update : ForceUpdateEnum.Value) : Unit =
    if (!update_list.contains(update))
      update_list = update :: update_list
  
  def add_update(updates : List[ForceUpdateEnum.Value]) : Unit =
    updates.foreach(add_update(_))
  
  def pop_update : List[ForceUpdateEnum.Value] = {
    val result = update_list
    update_list = List()
    result
  }
  
  def push_phase(roomphase : RoomPhase, is_room_save : Boolean = false) =
    RoomPhase.push(this, roomphase, is_room_save)
  
  def peek_phase : RoomPhase = 
    RoomPhase.peek(room, roomphases)
  
  def pop_phase(is_room_save : Boolean = false) : RoomPhase = 
    RoomPhase.pop(this, is_room_save)
  
  var delayed_action = false  // Action 因為觸發而移到之後再執行
  def is_delayed_action = delayed_action
  def set_delayed_action(x : Boolean) = delayed_action = x
  
  var new_round     = false  // 新的回合
  def is_new_round = new_round
  def set_new_round(x : Boolean) = new_round = x
  
  var next_turn     = false  // 強制跳到新的回合
  def is_next_turn = next_turn
  def set_next_turn(x : Boolean) = next_turn = x
  
  /*
  var first_action = false  // RoomPhase 一開始空的話，回合內 push 進 stack 的應該不要 pop
  def is_first_action = first_action
  def set_first_action(x : Boolean) = first_action = x
  
  
  
  //var no_pop = false  // 不 pop，同 first_action
  def is_no_pop = first_action
  def set_no_pop(x : Boolean) = first_action = x

  var addional_turn_checked = false  // 是否已檢查過額外回合
  def is_addional_turn_checked = addional_turn_checked
  def set_addional_turn_checked(x : Boolean) = addional_turn_checked = x
  */
  
  def roomphase = RoomPhase.get_phase(room, roomphases)
  
  //def refresh_roomphase = {
  //  roomphases = RoomPhase.findAllByRoomRoundId(roomround.id.is)
  //}
}

object EmptyGameObject extends GameObject(null, null, null, List(), List(), List())

