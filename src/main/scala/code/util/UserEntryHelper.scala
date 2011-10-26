package org.plummtw.astgrail.util

import _root_.net.liftweb._
import net.liftweb.mapper._
import http._
import SHtml._
import js._
import JsCmds._
import common._
import util._
import Helpers._

import scala.xml.NodeSeq

import org.plummtw.astgrail.model._
import org.plummtw.astgrail.enum._
import org.plummtw.astgrail.card._
import org.plummtw.astgrail.data._


object UserEntryHelper {
  def specific_cell(gameo : GameObject, userentry : UserEntry) = {
    var result : NodeSeq = Seq()
    val userentrys = gameo.userentrys
    userentrys.filter(_.get_role == RoleSealer).foreach { userentry1 => 
      if (userentry1.target_user.is == userentry.id.is)
        result ++= <span title="五系束縛">五</span>
    }
    //2011-10-25 zephyr : 修正名稱
    userentrys.filter(_.get_role == RoleSoulMage).foreach { userentry1 => 
      if (userentry1.target_user.is == userentry.id.is)
        result ++= <span title="靈魂鏈接">鏈</span>
    }
    userentrys.filter(_.get_role == RoleBrave).foreach { userentry1 => 
      if (userentry1.target_user.is == userentry.id.is)
        result ++= <span title="挑釁">挑</span>
    }
    userentrys.filter(_.get_role == RoleMiko).foreach { userentry1 => 
      if (userentry1.target_user.is == userentry.id.is)
        result ++= <span title="同生共死">同</span>
    }
    result
  }
  
  def hand_card_cell(hand_cards : List[CardPool], userentry : UserEntry, currentuserentry : UserEntry, reveal: Boolean ) = {
    if ((reveal || (userentry.id.is == currentuserentry.id.is)) && (hand_cards.length != 0)){
      val hand_cards1 = hand_cards.map(x => CardEnum.get_card(x.card.is))
      <a href="#" class="tip">{"手牌:" + hand_cards.length + "/" + userentry.get_hand_max.toString}<span>
        {for (card <- hand_cards1) yield Seq(card.card_name, <br/>)}</span></a>
    } else
      <span>{"手牌:" + hand_cards.length + "/" + userentry.get_hand_max.toString}</span>
  }
  
  def back_card_cell(back_cards : List[CardPool], userentry : UserEntry, currentuserentry : UserEntry, reveal: Boolean ) = {
    val role = userentry.get_role
    if (role.role_back_cards_max == 0)
      <span></span>
    else if ((reveal || (userentry.id.is == currentuserentry.id.is)) && (back_cards.length != 0)) {
      val back_cards1 = back_cards.map(x => CardEnum.get_card(x.card.is))
      <a href="#" class="tip">{"蓋牌:" + back_cards1.length.toString}<span>
        {for (card <- back_cards1) yield Seq(card.card_name.toString, <br/>)}</span></a>
    } else
      <span>{"蓋牌:" + back_cards.length.toString}</span>
  }
  
  def card_cell(gameo : GameObject, userentry : UserEntry) = {
    var result : NodeSeq= Seq()
    val front_cards = CardPool.in_front(userentry, gameo.card_list)
    val front_cards1 = front_cards.map(_.to_card)
    
    val poison_cards = front_cards1.filter(_.cardmagic_enum == CardMagicEnum.POISON)
    if (poison_cards.length != 0) {
      if (poison_cards.length == 1)
        result ++=  Seq(
          <a href="#" class="tip">毒<span>{for (card <- poison_cards) yield Seq(card.card_name.toString, <br/>)}</span></a>)
      else
        result ++=  Seq(
          <a href="#" class="tip">毒{poison_cards.length}<span>{for (card <- poison_cards) yield Seq(card.card_name.toString, <br/>)}</span></a>)
    }

    val weaken_cards = front_cards1.filter(_.cardmagic_enum == CardMagicEnum.WEAKEN)
    if (weaken_cards.length != 0) 
        result ++=  Seq(
          <a href="#" class="tip">虛<span>{for (card <- weaken_cards) yield Seq(card.card_name.toString, <br/>)}</span></a>)

    val shield_cards = front_cards1.filter(x => (x.cardmagic_enum == CardMagicEnum.SHIELD) ||
                                           (x.has_action(MTypeEnum.ACTION_ANGEL_ANGELICWALL)))
    if (shield_cards.length != 0) 
        result ++=  Seq(
          <a href="#" class="tip">盾<span>{for (card <- shield_cards) yield Seq(card.card_name.toString, <br/>)}</span></a>)

    //val bless_cards = front_cards1.filter(_.cardmagic_enum == CardMagicEnum.BLESS.toString)
    //if (bless_cards.length != 0) 
    //  result ++=  Seq(<span title={
    //    bless_cards.map(_.card_name).mkString("\\n")}>福</span>)
    //    
    val seal_cards = front_cards1.filter(_.has_action(MTypeEnum.ACTION_SEALER_SEAL))
    if (seal_cards.length != 0) {
      if (seal_cards.length == 1)
        result ++=  Seq(
          <a href="#" class="tip">封<span>{for (card <- seal_cards) yield Seq(card.card_name.toString, <br/>)}</span></a>)
      else
        result ++=  Seq(
          <a href="#" class="tip">封{seal_cards.length}<span>{for (card <- seal_cards) yield Seq(card.card_name.toString, <br/>)}</span></a>)
    }
    
    val powerbless_cards = front_cards1.filter(x => x.has_action(MTypeEnum.ACTION_PRAYER_POWERBLESS))
    if (powerbless_cards.length != 0) {
      if (powerbless_cards.length == 1)
        result ++=  Seq(
          <a href="#" class="tip">威<span>{for (card <- powerbless_cards) yield Seq(card.card_name.toString, <br/>)}</span></a>)
      else
        result ++=  Seq(
          <a href="#" class="tip">威{powerbless_cards.length}<span>{for (card <- powerbless_cards) yield Seq(card.card_name.toString, <br/>)}</span></a>)
    }
    
    val fastbless_cards = front_cards1.filter(x => x.has_action(MTypeEnum.ACTION_PRAYER_FASTBLESS))
    if (fastbless_cards.length != 0) {
      if (fastbless_cards.length == 1)
        result ++=  Seq(
          <a href="#" class="tip">迅<span>{for (card <- fastbless_cards) yield Seq(card.card_name.toString, <br/>)}</span></a>)
      else
        result ++=  Seq(
          <a href="#" class="tip">迅{fastbless_cards.length}<span>{for (card <- fastbless_cards) yield Seq(card.card_name.toString, <br/>)}</span></a>)
    }
    
    result
  }
  
  // http://74.82.5.143/
  // http://identicon.relucks.org/
  def user_cell(gameo : GameObject, currentuserentry: UserEntry, userentry: UserEntry, reveal: Boolean ) : NodeSeq = {
    val room = gameo.room
    val roomround = gameo.roomround
    val roomphase = RoomPhase.get_phase(room, gameo.roomphases)
    val card_list = gameo.card_list
    val hand_cards  = CardPool.in_hand(userentry, card_list)
    val back_cards  = CardPool.in_back(userentry, card_list)
    val userentryteams = gameo.userentryteams
    val role = userentry.get_role
    
    var result : NodeSeq = Seq()
    val user_icon : UserIcon = userentry.get_user_icon
    val id_icon : NodeSeq =
      if ((room.status.is != RoomStatusEnum.PLAYING.toString) || (userentry.ip_address.is != userentry.ip_address0.is))
        Seq(<img src={AdminHelper.identicon_link + userentry.ip_address_md5.is} />)
      else
        NodeSeq.Empty

    result ++= <td valign="top" class={if (userentry.has_room_flag(UserEntryRoomFlagEnum.VOTED)) "voted" else ""}>
    <img src={user_icon.icon_filename.is} width={user_icon.icon_width.is.toString} height={user_icon.icon_height.is.toString} border="2" style={"border-color:" + user_icon.color.is} /><br/>
    {hand_card_cell(hand_cards, userentry, currentuserentry, reveal)}<br/>
    {back_card_cell(back_cards, userentry, currentuserentry, reveal)}{specific_cell(gameo, userentry)}</td>

    // <img src={"http://identicon.relucks.org/" + user_entry.ip_address_md5.is} />
    val result2 = <td class={if (userentry.has_room_flag(UserEntryRoomFlagEnum.VOTED)) "voted" 
                             else if (userentry.id.is == roomround.actioner_id.is) "player-mark"
                             else if (userentry.id.is == roomphase.actioner_id.is) "player-mark2" 
                             else ""}>
          <font color={user_icon.color.is}>◆</font>{userentry.handle_name.is}<br/>
          { if (userentry.trip.is != "")
              Seq(<span>◆</span>, <a target="_blank" class="decor_none" href={AdminHelper.trip_link + userentry.trip.is}>{userentry.trip.is}</a>, <br/>)
            else
              NodeSeq.Empty }
          {id_icon}{ if ((roomphase.phase_type != RoomPhaseEnum.GAMEHALL.toString))
          //2011-10-22 zephyr:(update)加入角色說明link
             <strong><a target="_blank" title="點連結查看角色介紹" href={userentry.get_role_rule_link}>
             {userentry.get_role_field(UserEntryTeam.get(userentry.team_id.is, userentryteams).team_css)}</a></strong>
            else <strong>[？？]</strong>}
          {if (!role.role_use_tap) "" else if (userentry.tapped) "[橫]" else "[正]"}<br/>
          { "能" + userentry.gems.is.toString + "/" + userentry.crystals.toString}{
             if (role.role_yellow_index_max == 0)
               ""
             else if (role.role_blue_index_max == 0)
               " 指" + userentry.yellow_index.is.toString
             else
               " 指" + userentry.yellow_index.is.toString + "/" + userentry.blue_index.toString}{
             " 治" + userentry.heals.is.toString }<br/>{card_cell(gameo, userentry)}
         </td>
    
    result ++ result2
  }
  
  def user_admin_cell(room: Room, roomphase: RoomPhase, userentry: UserEntry ) : NodeSeq = {
    var result : NodeSeq = Seq()
    val user_icon : UserIcon = userentry.get_user_icon
    val id_icon : NodeSeq =
      if ((room.status.is != RoomStatusEnum.PLAYING.toString) || (userentry.ip_address.is != userentry.ip_address0.is))
        Seq(<img src={AdminHelper.identicon_link + userentry.ip_address_md5.is} />)
      else
        NodeSeq.Empty

    
    result
  }

  /*
  def user_select_cell(user_entry : UserEntry, targetable : Boolean ) : NodeSeq = {
    var result : NodeSeq = Seq()
    val user_icon : UserIcon = user_entry.get_user_icon()
    if (user_entry.live.is)
      result ++= <td class="table_votelist1" valign="top" bgcolor={if (user_entry.user_flags.is == UserEntryFlagEnum.VOTED.toString) "#FF50FF" else ""}>
      <img src={user_icon.icon_filename.is} width={user_icon.icon_width.is.toString} height={user_icon.icon_height.is.toString} border="2" style={"border-color:" + user_icon.color.is} /></td>
    else
      result ++= <td class="table_votelist1" valign="top" bgcolor="#992222"><img src="images/grave.gif"   border="2"  onMouseover={"this.src='" + user_icon.icon_filename.is + "'"} onMouseout="this.src='images/grave.gif'"  style={"border-color:" + user_icon.color.is} /></td>

    result ++= <td class="table_votelist2" width="150px" bgcolor={if (!user_entry.live.is) "#992222" else if (user_entry.user_flags.is == UserEntryFlagEnum.VOTED.toString) "#FF50FF" else ""}>
          {user_entry.handle_name.is}<br/><font color={user_icon.color.is}>◆</font>
          { if (targetable)
             Seq(<input type="radio" id="target" name="target" value={user_entry.id.is.toString} />)
            else
             NodeSeq.Empty}
         </td>
   return result
  }
  */

  def user_select_cell(userentry : UserEntry, node : NodeSeq ) : NodeSeq = {
    var result : NodeSeq = Seq()
    val user_icon : UserIcon = userentry.get_user_icon
    
    result ++= <td valign="top" class={if (userentry.has_room_flag(UserEntryRoomFlagEnum.VOTED)) "voted" else ""}>
    <img src={user_icon.icon_filename.is} width={user_icon.icon_width.is.toString} height={user_icon.icon_height.is.toString}
         border="2" style={"border-color:" + user_icon.color.is} /></td>

    result ++= <td class={if (userentry.has_room_flag(UserEntryRoomFlagEnum.VOTED)) "voted" else ""} width="150px" >
      {userentry.handle_name.is}<br/><font color={user_icon.color.is}>◆</font>
      {node}</td>
    result
  }
  
  // User Table
  def user_table(gameo: GameObject, currentuserentry:UserEntry, 
                 reveal: Boolean) : NodeSeq = {
    val userentrys_rr = UserEntry.rr(gameo.userentrys)
    val grouped_length  = if (userentrys_rr.length <= 6)  6
                          else 4
    val user_groups = userentrys_rr.grouped(grouped_length).toList
    //room:Room, current_user:UserEntry, user_entrys: List[UserEntry], 

    <table border="0" cellpadding="0" cellspacing="5" class="userentry_table"> <tbody>   
    { for (user_group <- user_groups) yield <tr> { 
       for (userentry <- user_group) yield user_cell(gameo, currentuserentry, userentry, reveal)
    } </tr> } </tbody></table> 
  }

  def user_select_table(userentrys: List[UserEntry], targettable_list: List[UserEntry], callback : String => Unit) : NodeSeq = {
    assert(! targettable_list.isEmpty)
        
    //val userentrys = UserEntrys_RR.get
    val user_groups = userentrys.grouped(5).toList
        
    val targettable_id_list = targettable_list.map(_.id.is.toString)
    val targettable_radios = SHtml.radio(targettable_id_list, Full(targettable_id_list(0)), callback(_))
    
        
    <table border="0" cellpadding="0" cellspacing="5" class="userentry_table"><tbody>
    { for (user_group <- user_groups) yield <tr> { 
       for (userentry <- user_group) yield {
         val index = targettable_list.indexOf(userentry)
         if (index != -1)
           user_select_cell(userentry, targettable_radios(index))
         else
           user_select_cell(userentry, NodeSeq.Empty)
       }
    } </tr> } </tbody></table> }
      
  def user_choose_table(userentrys: List[UserEntry], targettable_list: List[UserEntry], callback : Long => Unit) : NodeSeq = {
    assert(! targettable_list.isEmpty)
        
    //val userentrys = UserEntrys_RR.get
    val user_groups = userentrys.grouped(5).toList
        
    val targettable_id_list = targettable_list.map(_.id.is.toString)
    //val targettable_radios = SHtml.radio(targettable_id_list, Full(targettable_id_list(0)), callback(_))
    
        
    <table border="0" cellpadding="0" cellspacing="5" class="userentry_table"><tbody>
    { for (user_group <- user_groups) yield <tr> { 
       for (userentry <- user_group) yield {
         val index = targettable_list.indexOf(userentry)
         if (index != -1) {
           val targettable_checkbox = SHtml.checkbox(false, if (_) callback(userentry.id.is)) 
           user_select_cell(userentry, targettable_checkbox)
         } else
           user_select_cell(userentry, NodeSeq.Empty)
       }
    } </tr> } </tbody></table> }
          
          
  // User Admin Table
  def user_admin_table(room:Room, roomphase:RoomPhase, userentrys: List[UserEntry]) : NodeSeq = {
    val user_groups = userentrys.grouped(5).toList

    return <table border="0" cellpadding="0" cellspacing="5" style="userentry_table">
    { for (user_group <- user_groups) yield <tr> {
       for (userentry <- user_group) yield user_admin_cell(room, roomphase, userentry)
    } </tr> } </table> }

  /*
  // User Select Table
  def user_select_table(user_entrys : List[UserEntry], targettable : List[UserEntry]) : NodeSeq = {
    val user_groups = JinrouUtil.zipListBySize(5)(user_entrys)

    return <table border="0"><tr>
             <td width="1000">
              <table border="0">
    { for (val user_group <- user_groups) yield <tr> { 
       for (val user_entry <- user_group) yield user_select_cell(user_entry, targettable.contains(user_entry))
    } </tr> } </table></td></tr></table> }
   */
}


