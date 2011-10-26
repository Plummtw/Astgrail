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
import scala.xml.Unparsed

import org.plummtw.astgrail.enum._
import org.plummtw.astgrail.model._
import org.plummtw.astgrail.data._
import org.plummtw.astgrail.card._
import org.plummtw.astgrail.util._
import org.plummtw.astgrail.actor._

import org.plummtw.astgrail.heavy.GameProcessor

object MessageHelper extends Logger {
  // 一般言論
  def simple_talk_tag(talk:Talk, userentrys: List[UserEntry]) : NodeSeq= {
    val mtype : MTypeEnum.Value = 
      try { MTypeEnum.withName(talk.mtype.is) }
      catch { case e: Exception => MTypeEnum.TALK }
    //val font_size =
    //  try {talk.font_type.is.toInt}
    //  catch { case e:Exception => 0}
    //val grey_out_str = if (grey_out)  "color:#FFFFFF;background-color:#777777;" else ""
    //val style_str = if (font_size >= 20) "font-size:" + talk.font_type.is +"pt;font-weight:bold;"
   //                 else "font-size:" + talk.font_type.is +"pt;"
   // val user_entry_list  = user_entrys.filter(_.id.is == talk.actioner_id.is)
    var userentry = UserEntry.get(talk.actioner_id.is, userentrys) //UserEntrys_R.get
    val usericon  = userentry.get_user_icon

    mtype match {
      case MTypeEnum.TALK_DAY    =>
        Seq(<tr class="user-talk"><td class="user-name">
          <font color={usericon.color.is}>◆</font>{userentry.handle_name.is} </td>
          <td class={"say " + talk.cssclass.is}> {Unparsed(talk.message.is)} </td></tr>)
       case MTypeEnum.TALK_ADMIN    =>
        Seq(<tr class="admin-talk"><td class="user-name">
          <font color={usericon.color.is}>◆</font><span class="parrot">管理員</span> </td>
          <td class={"say " + talk.cssclass.is}> {Unparsed(talk.message.is)} </td></tr>)
       case MTypeEnum.TALK_ADMIN_PRIVATE    =>
        val useractionee = UserEntry.get(talk.actionee_id.is, userentrys)
        
        Seq(<tr class="admin-talk"><td class="user-name">
          <font color={usericon.color.is}>◆</font><span class="parrot">管理員</span>的悄悄話({useractionee.handle_name.is}) </td>
          <td class={"say " + talk.cssclass.is}> {Unparsed(talk.message.is)} </td></tr>)
        
      case _ => NodeSeq.Empty 
    }
  }
  
  def simple_message_tag(message: String) : NodeSeq= {
    //Seq(<tr><td width="1000" colspan="3" align="left" style="background-color:#efefef;color:black;font-weight:bold;border-top: silver 1px dashed;">　　　　　{message} </td></tr>)  
    Seq(<tr><td class="system-user" colspan="2">　　　　　{message}</td></tr>)
  }
  
  def simple_message_tag(message: String, reveal_mode: Boolean, cssclass : String) : NodeSeq= {
  //  val style_str = "background-color:" + background_color + ";color:" + color + ";" //font-weight:bold;border-top: silver 1px dashed;"  
    val css_str = if (cssclass == "") "system-user" else  cssclass 
  
    if (reveal_mode)
      //Seq(<tr><td width="1000" colspan="3" align="left" style={style_str}>　　　　　　　　　　　　{message} </td></tr>)
      Seq(<tr><td class={css_str} colspan="2">　　　　　{message}</td></tr>)
    else
      NodeSeq.Empty  
  }
  
  def talk_tag(talk: Talk, userentrys: List[UserEntry], reveal_mode: Boolean): NodeSeq = {
    val currentuserentry_id = CurrentUserEntry.get match {
      case Full(x) => x.id.is
      case x       => 0
    }

    val mtype : MTypeEnum.Value = 
      try { MTypeEnum.withName(talk.mtype.is) }
      catch { case e: Exception => MTypeEnum.TALK }
    
    val useractioner = UserEntry.get(talk.actioner_id.is, userentrys)
    val useractionee = UserEntry.get(talk.actionee_id.is, userentrys)
    
    /*
    val mtype : MTypeEnum.Value = MTypeEnum.valueOf(talk.mtype.is) getOrElse(null)
    val user_entry_list  = user_entrys.filter(_.id.is == talk.actioner_id.is)
    val user_target_list = user_entrys.filter(_.id.is == talk.actionee_id.is)

    var user_entry  : UserEntry = null
    var user_target : UserEntry = null

    //println("user_entry_list length : " +user_entry_list.length)
    var generated_message : String = ""

    //println("user_target_list length : " +user_target_list.length)
    if ((mtype == MTypeEnum.MESSAGE_COME) || (mtype == MTypeEnum.MESSAGE_LEAVE) || (mtype == MTypeEnum.MESSAGE_KICKED)) {
      var handle_name : String = ""
      if ((talk.message.is == null) || (talk.message.is == "")) {
        user_entry  = get_user_entry(user_entry_list, talk.actioner_id.is)
        handle_name = user_entry.handle_name.is
      } else
        handle_name = talk.message.is

      generated_message = handle_name + (mtype match {
        case MTypeEnum.MESSAGE_COME      => " 來到村莊大廳"
        case MTypeEnum.MESSAGE_LEAVE     => " 離開這個村莊了"
        case MTypeEnum.MESSAGE_KICKED    => " 人間蒸發、被轉學了"
        case xs                          => ""
      })
    } else if (mtype == MTypeEnum.VOTE_KICK) {
      if ((talk.message.is == null) || (talk.message.is == "")){
        user_entry  = get_user_entry(user_entry_list, talk.actioner_id.is)
        user_target = get_user_entry(user_target_list, talk.actionee_id.is)
        val user_entry_handle_name = (if (user_entry != null) user_entry.handle_name.is else "")
        val user_target_handle_name = (if (user_target != null) user_target.handle_name.is else "")
        generated_message = user_entry_handle_name + " 對 " + user_target_handle_name + " 投票踢出"
      } else
        generated_message = talk.message.is
    } else {
      user_entry  = get_user_entry(user_entry_list, talk.actioner_id.is)
      user_target = get_user_entry(user_target_list, talk.actionee_id.is)
    }
    */

    mtype match {
      case MTypeEnum.TALK_ADMIN          => simple_talk_tag(talk, userentrys)
      case MTypeEnum.TALK_ADMIN_PRIVATE  => //simple_talk_tag(room, room_day, talk, user, heaven_mode, user_entrys)
        if ((reveal_mode) || (currentuserentry_id == useractionee.id.is))
          simple_talk_tag(talk, userentrys)
        else
          Seq()
      case MTypeEnum.TALK_DAY            => simple_talk_tag(talk, userentrys)
      
      case MTypeEnum.MESSAGE_GENERAL     => simple_message_tag(talk.message.is)
      case MTypeEnum.MESSAGE_COME        => simple_message_tag(useractioner.handle_name.is + " 來到村莊大廳")
      case MTypeEnum.MESSAGE_LEAVE       => simple_message_tag(useractioner.handle_name.is +" 離開這個村莊了")
      case MTypeEnum.MESSAGE_KICKED      => simple_message_tag(useractioner.handle_name.is +" 人間蒸發、被轉學了")
      case MTypeEnum.MESSAGE_REVOTE0     => simple_message_tag("＜投票重新開始 請儘速重新投票＞")
      case MTypeEnum.MESSAGE_AUTO        => simple_message_tag(useractioner.handle_name.is +" 超過時限，系統自動行動")
      //case MTypeEnum.MESSAGE_DEATHSUDDEN => simple_message_tag(useractioner.handle_name.is + "  突然暴斃死亡")
      //case MTypeEnum.MESSAGE_REVOTE      => simple_message_tag("＜投票結果有問題 請重新投票＞")
      case MTypeEnum.MESSAGE_TIMEOUT     => simple_message_tag(useractioner.handle_name.is +" 超過時限，放棄回合")
      //case MTypeEnum.MESSAGE_DEATH       => simple_message_tag(useractioner.handle_name.is + "  淒慘的死狀被發現")
        
      //case MTypeEnum.OBJECTION_MALE                =>
      //    Seq(<tr><td class="objection-male" colspan="2">　　　　　{useractioner.handle_name.is} 要求廢村</td></tr>)
      //case MTypeEnum.OBJECTION_FEMALE              =>
      //    Seq(<tr><td class="objection-male" colspan="2">　　　　　{useractioner.handle_name.is} 要求廢村</td></tr>)
          
      case MTypeEnum.RESULT_ATTACK           => 
        simple_message_tag(useractioner.handle_name.is + " 受到 " + talk.message_flags.is + " 點攻擊傷害"
                           , true, "attack-do")
      case MTypeEnum.RESULT_REATTACK         => 
        simple_message_tag(useractioner.handle_name.is + " 受到 " + talk.message_flags.is + " 點攻擊(應戰)傷害"
                           , true, "attack-do")
      case MTypeEnum.RESULT_MAGIC            => 
        //if (talk.message_flags.is == RoomPhaseEnum.WEAKEN_REACTION.toString)
        //  simple_message_tag(useractioner.handle_name.is + " 受到虛弱傷害(3)", true, "magic-do")  
        //else  
          simple_message_tag(useractioner.handle_name.is + " 受到 " + talk.message_flags.is + " 點法術傷害"
                           , true, "magic-do")
      case MTypeEnum.RESULT_WEAKEN           => 
          simple_message_tag(useractioner.handle_name.is + " 受到虛弱傷害(" + talk.message_flags.is +  ")"
                           , true, "magic-do")
      case MTypeEnum.RESULT_POISON           => 
          simple_message_tag(useractioner.handle_name.is + " 受到中毒傷害(" + talk.message_flags.is +  ")"
                           , true, "magic-do")
      case MTypeEnum.RESULT_SEAL           => 
          simple_message_tag(useractioner.handle_name.is + " 受到封印傷害(" + CardAttrEnum.cname(talk.message_flags.is) +  ")"
                           , true, "magic-do")
      case MTypeEnum.RESULT_RETALIATE      => 
          simple_message_tag(useractioner.handle_name.is + " 受到反噬傷害", true, "magic-do")
        
      case MTypeEnum.ACTION_KICK             => simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 投票踢出", true, "kick-do")
      case MTypeEnum.ACTION_ATTACK           => 
        val actioner_role = useractioner.get_role
        val card = CardEnum.get_card(talk.message_flags.is)
        val additional_message = 
          if (actioner_role == RoleSwordSaint) {
            if (card.has_action(MTypeEnum.ACTION_SWORDSAINT_FASTGALE))
              "---疾風技"
            else if (card.has_action(MTypeEnum.ACTION_SWORDSAINT_STRONGGALE))
              "---裂風技"
            else ""
          } else if (actioner_role == RoleBerserker) {
            if (card.has_action(MTypeEnum.ACTION_BERSERKER_BLOODBLADE))
              "---血影狂刀"
            else if (card.has_action(MTypeEnum.ACTION_BERSERKER_BLOODROAR))
              "---血腥咆哮"
            else ""
          } else if (actioner_role == RoleArcher) {
            if (talk.message_flags2.is == MTypeEnum.ACTION_ARCHER_AIM.toString)
              "---精準射擊"
            else ""
          } else if (actioner_role == RoleSaintGirl) {
            if (card.cardattr_enum == CardAttrEnum.WATER)
              "---冰霜導言：" + UserEntry.get(PlummUtil.parseLong(talk.message_flags2.is), userentrys).handle_name.is
            else ""
          } else if (actioner_role == RoleSaintLance) {
            if (talk.message_flags2.is == MTypeEnum.ACTION_SAINTLANCE_SKYLANCE.toString)
              "---天槍"
            else ""
          } else if (actioner_role == RoleMagicSword) {
            if (talk.message_flags2.is == MTypeEnum.ACTION_MAGICSWORD_DARKSTUN.toString)
              "---黑暗震顫"
            else ""
          } else if (actioner_role == RoleSwordEmp) {
            if (talk.message_flags2.is == MTypeEnum.ACTION_SWORDEMP_ANGELSOUL.toString)
              "---天使之魂"
            else if (talk.message_flags2.is == MTypeEnum.ACTION_SWORDEMP_DEVILSOUL.toString)
              "---惡魔之魂"
            else ""
          } else if (actioner_role == RoleMonk) {
            if (talk.message_flags2.is == MTypeEnum.ACTION_MONK_POWERUP.toString)
              "---蓄力一擊"
            else if (talk.message_flags2.is == MTypeEnum.ACTION_MONK_FIRESOUL.toString)
              "---蒼炎之魂"
            else ""
          } else if (actioner_role == RoleBrave) {
            var result = ""
            val talkflags = talk.message_flags2.is.split(",")
            talkflags.foreach { talkflag =>
              if (talkflag == MTypeEnum.ACTION_BRAVE_ROAR.toString)
                result += "---怒吼"
              else if (talkflag == MTypeEnum.ACTION_BRAVE_STILLWATER.toString)
                result += "---明鏡止水"
            }
            result
          } else if (actioner_role == RoleSoulMage) {
            if (talk.message_flags2.is == MTypeEnum.ACTION_SOULMAGE_SOULCONVERTY.toString)
              "---黃轉藍"
            else if (talk.message_flags2.is == MTypeEnum.ACTION_SOULMAGE_SOULCONVERTB.toString)
              "---藍轉黃"
            else ""
          } else ""

        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 攻擊！！(" +
                           card.card_name + ")" + additional_message
                           , true, "shadow-do")
      case MTypeEnum.ACTION_REATTACK           => 
        val card = CardEnum.get_card(talk.message_flags.is)
        val additional_message = 
          if (useractioner.get_role == RoleArcher) {
            if (talk.message_flags2.is == MTypeEnum.ACTION_ARCHER_AIM.toString)
              "---精準射擊"
            else ""
          } else if (useractioner.get_role == RoleSaintGirl) {
            if (card.cardattr_enum == CardAttrEnum.WATER)
              "---冰霜導言：" + UserEntry.get(PlummUtil.parseLong(talk.message_flags2.is), userentrys).handle_name.is
            else ""
          } else ""
        
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 應戰攻擊！！(" +
                           card.card_name + ")" + additional_message
                           , true, "attack-do")
      case MTypeEnum.ACTION_MAGIC              => 
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用魔法！！(" +
                           CardEnum.get_card(talk.message_flags.is).card_name + ")"
                           , true, "magic-do2")
      case MTypeEnum.ACTION_MAGIC_POISON           => 
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用中毒！！(" +
                           CardEnum.get_card(talk.message_flags.is).card_name + ")"
                           , true, "magic-do2")  
      case MTypeEnum.ACTION_MAGIC_WEAKEN           => 
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用虛弱！！(" +
                           CardEnum.get_card(talk.message_flags.is).card_name + ")"
                           , true, "magic-do2")  
      case MTypeEnum.ACTION_MAGIC_SHIELD           => 
        val additional_message =
          if (useractioner.get_role == RoleAngel)
            "(天使羈絆：" + UserEntry.get(PlummUtil.parseLong(talk.message_flags2.is), userentrys).handle_name + ")"
          else ""
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用聖盾！！(" +
                           CardEnum.get_card(talk.message_flags.is).card_name + ")" + additional_message
                           , true, "magic-do2")  
      case MTypeEnum.ACTION_MAGIC_MBOLT           => 
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用魔彈！！(" +
                           CardEnum.get_card(talk.message_flags.is).card_name + ")"
                           , true, "magic-do2")  
        
      case MTypeEnum.ACTION_LIGHT              =>
        val cssclass = 
          if ((talk.message_flags2.is == RoomPhaseEnum.ATTACK.toString) ||
              (talk.message_flags2.is == RoomPhaseEnum.REATTACK.toString))
            "attack-do"
          else
            "magic-do"
        simple_message_tag(useractioner.handle_name.is + " 以聖光抵擋(" + 
          CardEnum.get_card(talk.message_flags.is).card_name + ")", true, cssclass)

      case MTypeEnum.ACTION_SHIELD             =>
        simple_message_tag(useractioner.handle_name.is + " 以聖盾抵擋", true, "attack-do")

      //case MTypeEnum.ACTION_ENDUREATTACK       =>
      //  simple_message_tag(useractioner.handle_name.is + " 承受攻擊", true, "attack-do")
        
      //case MTypeEnum.ACTION_ENDUREMAGIC        =>
      //  simple_message_tag(useractioner.handle_name.is + " 承受法術", true, "magic-do")  

      case MTypeEnum.ACTION_SKIPTURN           =>
        simple_message_tag(useractioner.handle_name.is + " 跳過回合", true, "neutral-do")
        
      case MTypeEnum.ACTION_HEAL               =>
        val cssclass = 
          if ((talk.message_flags2.is == RoomPhaseEnum.ATTACK.toString) ||
              (talk.message_flags2.is == RoomPhaseEnum.REATTACK.toString))
            "attack-do"
          else
            "magic-do"
        simple_message_tag(useractioner.handle_name.is + " 治療(" + talk.message_flags.is + ")", true, cssclass)  
        
      case MTypeEnum.ACTION_PURCHASE           => 
        simple_message_tag(useractioner.handle_name.is + " 執行購買", true, "neutral-do")
      case MTypeEnum.ACTION_COMBINE           => 
        simple_message_tag(useractioner.handle_name.is + " 執行合成(" +
                           GameEnum.combine_cname(talk.message_flags.is) + ")"
                           , true, "neutral-do")
      case MTypeEnum.ACTION_REFINE           => 
        val message = 
          if (useractioner.get_role == RoleAdventurer) "，並給予 " + useractionee.handle_name.is
          else ""
        simple_message_tag(useractioner.handle_name.is + " 執行提煉(" +
                           GameEnum.combine_cname(talk.message_flags.is) + ")" + message
                           , true, "neutral-do")
      case MTypeEnum.ACTION_DISCARD          => 
        val message = 
          if (reveal_mode || (useractioner.id.is == currentuserentry_id))
            "(" + talk.message_flags.is.split(",").toList.map(x => CardEnum.get_card(x).card_name).mkString(",") + ")"
          else
            "(" + talk.message_flags.is.split(",").toList.length + "張)"
        simple_message_tag(useractioner.handle_name.is + " 丟棄卡片" + message , true, "black-do")
      case MTypeEnum.ACTION_CARD_RENEW        => 
        val message = 
            "(" + talk.message_flags.is.split(",").toList.map(x => CardEnum.get_card(x).card_name).mkString(",") + ")"
        simple_message_tag(useractioner.handle_name.is + " 重洗手牌" + message , true, "neutral-do")

      case MTypeEnum.ACTION_SWORDSAINT_COMBO =>
        simple_message_tag(useractioner.handle_name.is + " 使用連續技" , true, "hunter-do")
      case MTypeEnum.ACTION_SWORDSAINT_SWORDSHADOW =>
        simple_message_tag(useractioner.handle_name.is + " 使用劍影" , true, "hunter-do")
      case MTypeEnum.ACTION_BERSERKER_LACERATE =>
        simple_message_tag(useractioner.handle_name.is + " 使用撕裂" , true, "hunter-do")
      case MTypeEnum.ACTION_ARCHER_TRAP =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用閃光陷阱(" +
                           CardEnum.get_card(talk.message_flags.is).card_name + ")" , true, "hunter-do")
      case MTypeEnum.ACTION_ARCHER_SNIPE =>
        simple_message_tag(useractioner.handle_name.is + " 對 "  + useractionee.handle_name.is + " 狙擊", true, "hunter-do")  
      case MTypeEnum.ACTION_ARCHER_PENETRATE =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 貫穿射擊(" +
                           CardEnum.get_card(talk.message_flags.is).card_name + ")" , true, "hunter-do")
      case MTypeEnum.ACTION_ASSASSIN_WATERSHADOW    => 
        val message = 
          "(" + talk.message_flags.is.split(",").toList.map(x => CardEnum.get_card(x).card_name).mkString(",") + ")"
        simple_message_tag(useractioner.handle_name.is + " 使用水影" + message , true, "hunter-do")  
      case MTypeEnum.ACTION_ASSASSIN_SNEAK =>
        simple_message_tag(useractioner.handle_name.is + " 開始潛行" , true, "hunter-do")
      case MTypeEnum.ACTION_SAINTGIRL_HEAL =>
        simple_message_tag(useractioner.handle_name.is + " 對 "  + useractionee.handle_name.is + " 使用治療術(" +
                           CardEnum.get_card(talk.message_flags.is).card_name + ")", true, "hunter-do")  
      case MTypeEnum.ACTION_SAINTGIRL_HEALLIGHT =>
        val actionees = 
          try { talk.message_flags2.split(",").toList.map(x => UserEntry.get(x.toLong, userentrys).handle_name.is) }
          catch {case e: Exception => List() }
        simple_message_tag(useractioner.handle_name.is + " 對 "  + actionees.mkString(",") + " 使用治癒之光(" +
                           CardEnum.get_card(talk.message_flags.is).card_name + ")", true, "hunter-do")  
      case MTypeEnum.ACTION_SAINTGIRL_COMPASSION =>
        simple_message_tag(useractioner.handle_name.is + " 使用憐憫" , true, "hunter-do")  
      case MTypeEnum.ACTION_SAINTGIRL_HOLYHEAL =>
        val actionees = 
          try { talk.message_flags2.split(",").toList.map(x => UserEntry.get(x.toLong, userentrys).handle_name.is) }
          catch {case e: Exception => List() }
        simple_message_tag(useractioner.handle_name.is + " 對 "  + actionees.mkString(",") + " 使用聖療", true, "hunter-do")  
      case MTypeEnum.ACTION_SEALER_SEAL =>
        simple_message_tag(useractioner.handle_name.is + " 對 "  + useractionee.handle_name.is + " 封印(" +
                           CardEnum.get_card(talk.message_flags.is).card_name + ")", true, "hunter-do")  
      case MTypeEnum.ACTION_SEALER_FIVESEAL =>
        simple_message_tag(useractioner.handle_name.is + " 對 "  + useractionee.handle_name.is + " 五系束縛", true, "hunter-do")  
      case MTypeEnum.ACTION_SEALER_BREAKSEAL =>
        simple_message_tag(useractioner.handle_name.is + " 破碎 "  + useractionee.handle_name.is + " 的效果(" +
                           CardEnum.get_card(talk.message_flags.is).card_name + ")", true, "hunter-do")  
      case MTypeEnum.ACTION_ANGEL_WINDPURIFY =>
        simple_message_tag(useractioner.handle_name.is + " 使用風之潔淨("  + CardEnum.get_card(talk.message_flags.is).card_name + ")，消除效果(" +
                           CardEnum.get_card(talk.message_flags2.is).card_name + ")(天使羈絆：" +
                           useractionee.handle_name.is + ")", true, "hunter-do")  
      case MTypeEnum.ACTION_ANGEL_ANGELBLESS =>
        val actionees = 
          try { talk.message_flags2.split(",").toList.map(x => UserEntry.get(x.toLong, userentrys).handle_name.is) }
          catch {case e: Exception => List() }
        simple_message_tag(useractioner.handle_name.is + " 對 " + actionees.mkString(",") + 
                           " 使用天使祝福("  + CardEnum.get_card(talk.message_flags.is).card_name + ")"
                           , true, "hunter-do")  
      case MTypeEnum.ACTION_ANGEL_ANGELBLESS_GIVE =>
        val message = 
          if (reveal_mode || (useractioner.id.is == currentuserentry_id) || (useractionee.id.is == currentuserentry_id))
            "(" + talk.message_flags.is.split(",").toList.map(x => CardEnum.get_card(x).card_name).mkString(",") + ")"
          else
            "(" + talk.message_flags.is.split(",").toList.length + "張)"
        simple_message_tag(useractioner.handle_name.is + " 給予 "  + useractionee.handle_name.is + message, true, "hunter-do")  
      case MTypeEnum.ACTION_ANGEL_ANGELSONG =>
        simple_message_tag(useractioner.handle_name.is + " 使用天使之歌，消除效果(" +
                           CardEnum.get_card(talk.message_flags.is).card_name + ")(天使羈絆：" +
                           useractionee.handle_name.is + ")", true, "hunter-do")  
      case MTypeEnum.ACTION_ANGEL_GODCOVER =>
        simple_message_tag(useractioner.handle_name.is + " 使用神之庇護(" + talk.message_flags.is +
                           ")", true, "hunter-do") 
      case MTypeEnum.ACTION_MAGE_SHOCK =>
        val actionees = 
          try { talk.message_flags2.split(",").toList.map(x => UserEntry.get(x.toLong, userentrys).handle_name.is) }
          catch {case e: Exception => List() }
        simple_message_tag(useractioner.handle_name.is + " 對 " + actionees.mkString(",") + 
                           " 使用魔爆衝擊("  + CardEnum.get_card(talk.message_flags.is).card_name + ")"
                           , true, "hunter-do") 
      case MTypeEnum.ACTION_MAGE_SHOCK_DISCARD =>
        simple_message_tag(useractioner.handle_name.is + " 丟棄 " + CardEnum.get_card(talk.message_flags.is).card_name 
                           , true, "hunter-do")
      case MTypeEnum.ACTION_MAGE_STORM        =>
        val actionees = 
          try { talk.message_flags2.split(",").toList.map(x => UserEntry.get(x.toLong, userentrys).handle_name.is) }
          catch {case e: Exception => List() }
        simple_message_tag(useractioner.handle_name.is + " 對 " + actionees.mkString(",") + 
                           " 使用毀滅風暴", true, "hunter-do")  
        
      case MTypeEnum.ACTION_SAINTLANCE_SHINE =>
        simple_message_tag(useractioner.handle_name.is + " 使用輝耀("  + CardEnum.get_card(talk.message_flags.is).card_name + ")"
                           , true, "hunter-do")   
      case MTypeEnum.ACTION_SAINTLANCE_SMITE       =>
        simple_message_tag(useractioner.handle_name.is + " 使用聖擊"
                           , true, "hunter-do") 
      case MTypeEnum.ACTION_SAINTLANCE_RETRIBUTION =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + 
                           " 使用懲戒("  + CardEnum.get_card(talk.message_flags.is).card_name + ")"
                           , true, "hunter-do") 
      case MTypeEnum.ACTION_SAINTLANCE_EARTHLANCE  =>
        simple_message_tag(useractioner.handle_name.is + " 使用地槍("  + talk.message_flags.is + ")"
                           , true, "hunter-do") 
      case MTypeEnum.ACTION_SAINTLANCE_HOLYPRAY    =>
        simple_message_tag(useractioner.handle_name.is + " 使用聖光祈癒", true, "hunter-do") 
      case MTypeEnum.ACTION_ELEMENTALIST_MAGIC =>
        val cards = talk.message_flags.is.split(",").toList.map(x => CardEnum.get_card(x))
        val card1_str = 
          if (cards(0).cardattr_enum == CardAttrEnum.FIRE)         "火球"
          else if (cards(0).cardattr_enum == CardAttrEnum.WATER)   "冰凍"
          else if (cards(0).cardattr_enum == CardAttrEnum.AIR)     "風刃"
          else if (cards(0).cardattr_enum == CardAttrEnum.THUNDER) "雷擊"
          else if (cards(0).cardattr_enum == CardAttrEnum.EARTH)   "隕石"
          else  "？？"
        val card2_str =
          if (cards.length > 1) "(並棄牌" + cards(1).card_name + ")"
          else ""
        val ice_str =
          if (cards(0).cardattr_enum == CardAttrEnum.WATER) "(冰凍治癒：" + UserEntry.get(PlummUtil.parseLong(talk.message_flags2.is), userentrys).handle_name + ")"
          else ""
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + 
                           " 使用" + card1_str + "("  + cards(0).card_name + ")" + card2_str + ice_str
                           , true, "hunter-do") 
      case MTypeEnum.ACTION_ELEMENTALIST_IGNITE =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + 
                           " 使用元素點燃" , true, "hunter-do") 
      case MTypeEnum.ACTION_ELEMENTALIST_MOONLIGHT =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + 
                           " 使用月光" , true, "hunter-do") 
      case MTypeEnum.ACTION_NECROMANCER_PLAGUE =>
        simple_message_tag(useractioner.handle_name.is + " 使用瘟疫("  + 
                           CardEnum.get_card(talk.message_flags.is).card_name + ")" , true, "hunter-do")   
      case MTypeEnum.ACTION_NECROMANCER_DEATHTOUCH =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + 
                           " 使用死亡之觸(" + talk.message_flags.is.split(",").toList.map(x => CardEnum.get_card(x).card_name).mkString(",") +
                           ")(使用治療：" + talk.message_flags2.is + ")"
                           , true, "hunter-do") 
      case MTypeEnum.ACTION_NECROMANCER_GRAVEFALL =>
        simple_message_tag(useractioner.handle_name.is + " 使用墓碑隕落" , true, "hunter-do")   
      case MTypeEnum.ACTION_MAGICSWORD_GATHER =>
        simple_message_tag(useractioner.handle_name.is + " 使用暗影凝聚" , true, "hunter-do")   
      case MTypeEnum.ACTION_MAGICSWORD_COMET =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + 
                           " 使用暗影流星(" + talk.message_flags.is.split(",").toList.map(x => CardEnum.get_card(x).card_name).mkString(",") +
                           ")", true, "hunter-do") 
      case MTypeEnum.ACTION_JUDICATOR_BREAKRITUAL  =>
        simple_message_tag(useractioner.handle_name.is + " 使用儀式中斷" , true, "hunter-do")   
      case MTypeEnum.ACTION_JUDICATOR_FINALJUDGE   =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用末日審判" , true, "hunter-do")   
      case MTypeEnum.ACTION_JUDICATOR_RITUAL       =>
        simple_message_tag(useractioner.handle_name.is + " 使用仲裁儀式" , true, "hunter-do")   
      case MTypeEnum.ACTION_JUDICATOR_BALANCE      =>
        val additional =
          if (talk.message_flags.is == "1") "(棄牌)"
          else if (talk.message_flags.is == "2") "(補牌)"
          else "(？？)"
        simple_message_tag(useractioner.handle_name.is + " 使用判決天平" + additional, true, "hunter-do")   
        
      case MTypeEnum.ACTION_ADVENTURER_DECEIVE     =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + 
                           " 詐欺(" + talk.message_flags.is.split(",").toList.map(x => CardEnum.get_card(x).card_name).mkString(",") +
                           ")(屬性：" + CardAttrEnum.cname(talk.message_flags2.is) + ")"
                           , true, "hunter-do") 
      case MTypeEnum.ACTION_ADVENTURER_ADDON       =>
        simple_message_tag(useractioner.handle_name.is + " 使用特殊加工" , true, "hunter-do")   
      case MTypeEnum.ACTION_ADVENTURER_THEFT       =>
        simple_message_tag(useractioner.handle_name.is + " 使用偷天換日" , true, "hunter-do")   
  
      case MTypeEnum.ACTION_BISHOP_HOLYBLESS      =>
        simple_message_tag(useractioner.handle_name.is + " 使用神聖祈福(" + talk.message_flags.is.split(",").toList.map(x => CardEnum.get_card(x).card_name).mkString(",") +
                           ")", true, "hunter-do") 
      case MTypeEnum.ACTION_BISHOP_HOLYWATER      =>
        val message = 
          if (reveal_mode || (useractioner.id.is == currentuserentry_id))
            "(" + CardEnum.get_card(talk.message_flags2.is).card_name + ")"
          else "(1張)"
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用水之神力(" + CardEnum.get_card(talk.message_flags.is).card_name +
                           ")" + message , true, "hunter-do") 
      case MTypeEnum.ACTION_BISHOP_HOLYCONTRACT   =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用神聖契約(" + talk.message_flags.is +
                           ")", true, "hunter-do") 
      case MTypeEnum.ACTION_BISHOP_HOLYFIELD      =>
        val message = 
          if (reveal_mode || (useractioner.id.is == currentuserentry_id))
            "(" + talk.message_flags.is.split(",").toList.map(x => CardEnum.get_card(x).card_name).mkString(",") + ")"
          else
            "(" + talk.message_flags.is.split(",").toList.length + "張)"
        val additional =
          if (talk.message_flags2.is == "1") "(法術傷害)"
          else if (talk.message_flags2.is == "2") "(增加治癒)"
          else "(？？)"
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + 
                           " 使用神聖領域" + message +
                            additional , true, "hunter-do") 
      case MTypeEnum.ACTION_SAGE_REFLECT           =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + 
                           " 使用法術反彈(" + talk.message_flags.is.split(",").toList.map(x => CardEnum.get_card(x).card_name).mkString(",") +
                           ")", true, "hunter-do") 
      case MTypeEnum.ACTION_SAGE_MAGICBOOK         =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + 
                           " 使用魔道法典(" + talk.message_flags.is.split(",").toList.map(x => CardEnum.get_card(x).card_name).mkString(",") +
                           ")", true, "hunter-do") 
      case MTypeEnum.ACTION_SAGE_HOLYBOOK          =>
        val actionees = 
          try { talk.message_flags2.split(",").toList.map(x => UserEntry.get(x.toLong, userentrys).handle_name.is) }
          catch {case e: Exception => List() }
        simple_message_tag(useractioner.handle_name.is + " 對 " + actionees.mkString(",") + 
                           " 使用聖潔法典(" + talk.message_flags.is.split(",").toList.map(x => CardEnum.get_card(x).card_name).mkString(",") +
                           ")", true, "hunter-do") 
      case MTypeEnum.ACTION_RUNEMAGE_THUNDERRUNE   =>
        val message_flags = talk.message_flags.is.split(",")
        val card          = CardEnum.get_card(message_flags(0))
        val card2         = CardEnum.get_card(message_flags(1))
        val message2 =  
          if (card2 == CardNone) ""
          else if (reveal_mode || (useractioner.id.is == currentuserentry_id))
            "(蓋牌：" + card2.card_name + ")"
          else "(蓋牌：1 張)"
        val message3 =
          if ((message_flags.length > 2) && (message_flags(2) == MTypeEnum.ACTION_RUNEMAGE_USERUNE.toString)) "--靈力崩解"
          else ""
            
        val actionees = 
          try { talk.message_flags2.split(",").toList.map(x => UserEntry.get(x.toLong, userentrys).handle_name.is) }
          catch {case e: Exception => List() }
        simple_message_tag(useractioner.handle_name.is + " 對 " + actionees.mkString(",") + 
                           " 使用靈符雷鳴(" + card.card_name  + ")" + message2 + message3 , true, "hunter-do") 
      case MTypeEnum.ACTION_RUNEMAGE_AIRRUNE       =>
        val message_flags = talk.message_flags.is.split(",")
        val card          = CardEnum.get_card(message_flags(0))
        val card2         = CardEnum.get_card(message_flags(1))
        val message2 =  
          if (card2 == CardNone) ""
          else if (reveal_mode || (useractioner.id.is == currentuserentry_id))
            "(蓋牌：" + card2.card_name + ")"
          else "(蓋牌：1 張)"
            
        val actionees = 
          try { talk.message_flags2.split(",").toList.map(x => UserEntry.get(x.toLong, userentrys).handle_name.is) }
          catch {case e: Exception => List() }
        simple_message_tag(useractioner.handle_name.is + " 對 " + actionees.mkString(",") + 
                           " 使用靈符風行(" + card.card_name  + ")" + message2, true, "hunter-do")
      case MTypeEnum.ACTION_RUNEMAGE_AIRRUNE_DISCARD =>
        if (reveal_mode || (useractioner.id.is == currentuserentry_id) || (useractionee.id.is == currentuserentry_id))
          simple_message_tag(useractioner.handle_name.is + " 丟棄 " + CardEnum.get_card(talk.message_flags.is).card_name 
                           , true, "hunter-do")
        else
          simple_message_tag(useractioner.handle_name.is + " 丟棄 1 張", true, "hunter-do")
      case MTypeEnum.ACTION_RUNEMAGE_100GHOSTS     =>
        val message_flags = talk.message_flags.is.split(",")
        val card          = CardEnum.get_card(message_flags(0))
        val message2 =
          if ((message_flags.length > 1) && (message_flags(1) == MTypeEnum.ACTION_RUNEMAGE_USERUNE.toString)) "--靈力崩解"
          else ""
            
        val actionees = 
          try { talk.message_flags2.split(",").toList.map(x => UserEntry.get(x.toLong, userentrys).handle_name.is) }
          catch {case e: Exception => List() }
        simple_message_tag(useractioner.handle_name.is + " 對 " + actionees.mkString(",") + 
                           " 使用百鬼夜行(移除蓋牌：" + card.card_name  + ")" + message2  , true, "hunter-do") 
      case MTypeEnum.ACTION_PRAYER_POWERBLESS     =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用威力祈福(" +
                           CardEnum.get_card(talk.message_flags.is).card_name + ")", true, "hunter-do")
      case MTypeEnum.ACTION_PRAYER_POWERBLESS_USE =>
        simple_message_tag(useractioner.handle_name.is + " 發動威力祈福", true, "hunter-do")
      case MTypeEnum.ACTION_PRAYER_FASTBLESS      =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用迅捷祈福(" +
                           CardEnum.get_card(talk.message_flags.is).card_name + ")", true, "hunter-do")
      case MTypeEnum.ACTION_PRAYER_FASTBLESS_USE  =>
        simple_message_tag(useractioner.handle_name.is + " 發動迅捷祈福", true, "hunter-do")
      case MTypeEnum.ACTION_PRAYER_SHINEBELIEVE   =>
        val message = 
          if (reveal_mode || (useractioner.id.is == currentuserentry_id))
            "(" + talk.message_flags.is.split(",").toList.map(x => CardEnum.get_card(x).card_name).mkString(",") + ")"
          else
            "(" + talk.message_flags.is.split(",").toList.length + "張)"
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用光輝信仰" + message, true, "hunter-do")
      case MTypeEnum.ACTION_PRAYER_DARKBELIEVE    =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用漆黑信仰", true, "hunter-do")
      case MTypeEnum.ACTION_PRAYER_PRAY           =>
        simple_message_tag(useractioner.handle_name.is + " 使用祈禱", true, "hunter-do")
      case MTypeEnum.ACTION_PRAYER_MANATIDE       =>
        simple_message_tag(useractioner.handle_name.is + " 使用法力潮汐", true, "hunter-do")
      case MTypeEnum.ACTION_SWORDEMP_SWORDKI    =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用劍氣斬(" + talk.message_flags.is + ")", true, "hunter-do")
      case MTypeEnum.ACTION_SWORDEMP_LIBIDINALWILL    =>
        simple_message_tag(useractioner.handle_name.is + " 使用不屈意志", true, "hunter-do")
      case MTypeEnum.ACTION_MONK_KISHOT    =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用念彈" , true, "hunter-do")
      case MTypeEnum.ACTION_MONK_100DRAGONS    =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用百式幻龍拳" , true, "hunter-do")
      case MTypeEnum.ACTION_MONK_100DRAGONS_REMOVE    =>
        simple_message_tag(useractioner.handle_name.is + " 取消百式幻龍拳" , true, "hunter-do")
      case MTypeEnum.ACTION_MONK_MONKGOD           =>
        val message = 
          if (reveal_mode || (useractioner.id.is == currentuserentry_id))
            "(" + talk.message_flags.is.split(",").toList.map(x => CardEnum.get_card(x).card_name).mkString(",") + ")"
          else
            "(" + talk.message_flags.is.split(",").toList.length + "張)"
        simple_message_tag(useractioner.handle_name.is + " 使用鬥神天驅" + message , true, "hunter-do")
      case MTypeEnum.ACTION_BRAVE_TAUNT    =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 挑釁" , true, "hunter-do")
      case MTypeEnum.ACTION_BRAVE_FORBIDDEN    =>
        simple_message_tag(useractioner.handle_name.is + " 使用禁斷之力(" +
                           talk.message_flags.is.split(",").toList.map(x => CardEnum.get_card(x).card_name).mkString(",") + ")",
                           true, "hunter-do")
      case MTypeEnum.ACTION_BRAVE_DEATHMATCH    =>
        simple_message_tag(useractioner.handle_name.is + " 使用死鬥" , true, "hunter-do")
      case MTypeEnum.ACTION_REDKNIGHT_BLOODPRAY    =>
        val actionees = 
          try { talk.message_flags.split(",").toList.map(x => UserEntry.get(x.toLong, userentrys).handle_name.is) }
          catch {case e: Exception => List() }
        simple_message_tag(useractioner.handle_name.is + " 對 " + actionees.mkString(",") + " 使用血腥導言(" + talk.message_flags2.is + ")",
                           true, "hunter-do")
      case MTypeEnum.ACTION_REDKNIGHT_BLOODFEAST    =>
        simple_message_tag(useractioner.handle_name.is + " 使用殺戮盛宴" , true, "hunter-do")
      case MTypeEnum.ACTION_REDKNIGHT_DISCIPLINE    =>
        simple_message_tag(useractioner.handle_name.is + " 使用戒驕戒躁" , true, "hunter-do")
      case MTypeEnum.ACTION_REDKNIGHT_BLOODCROSS    =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用腥紅十字(" + 
          talk.message_flags.is.split(",").toList.map(x => CardEnum.get_card(x).card_name).mkString(",") + ")", true, "hunter-do")

      case MTypeEnum.ACTION_SOULMAGE_SOULGIVE       =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用靈魂賜予" , true, "hunter-do")
      case MTypeEnum.ACTION_SOULMAGE_SOULBURST      =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用靈魂震爆" , true, "hunter-do")
      case MTypeEnum.ACTION_SOULMAGE_SOULSUMMON     =>
        simple_message_tag(useractioner.handle_name.is + " 使用靈魂召還(" + 
          talk.message_flags.is.split(",").toList.map(x => CardEnum.get_card(x).card_name).mkString(",") + ")" , true, "hunter-do")
      case MTypeEnum.ACTION_SOULMAGE_SOULMIRROR     =>
        val message = 
          if (reveal_mode || (useractioner.id.is == currentuserentry_id))
            "(" + talk.message_flags.is.split(",").toList.map(x => CardEnum.get_card(x).card_name).mkString(",") + ")"
          else
            "(" + talk.message_flags.is.split(",").toList.length + "張)"
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用靈魂鏡像" + 
          message, true, "hunter-do")
      //2011-10-25 zephyr : 修正名稱          
      case MTypeEnum.ACTION_SOULMAGE_SOULLINK          =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用靈魂鏈接" , true, "hunter-do")
      case MTypeEnum.ACTION_SOULMAGE_SOULLINK_TRANSFER =>
        if (talk.message_flags2.is == SoulMageEnum.FROMMAGE.toString)
          simple_message_tag(useractioner.handle_name.is + " 將傷害轉移至 " + useractionee.handle_name.is + " (" + talk.message_flags.is + ")" , true, "hunter-do")
        else
          simple_message_tag(useractioner.handle_name.is + " 轉移 " + useractionee.handle_name.is + " 的傷害至自己(" + talk.message_flags.is + ")" , true, "hunter-do")
      case MTypeEnum.ACTION_SOULMAGE_SOULENHANCE     =>
        simple_message_tag(useractioner.handle_name.is + " 使用靈魂增幅" , true, "hunter-do")
      case MTypeEnum.ACTION_MIKO_BLOODCRY          =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用血之悲鳴(" +
                           CardEnum.get_card(talk.message_flags.is).card_name + ")(" + talk.message_flags2.is + ")" , true, "hunter-do")
      case MTypeEnum.ACTION_MIKO_STICKWITH         =>
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用同生共死" , true, "hunter-do")
      case MTypeEnum.ACTION_MIKO_BLOODSORROW       =>
        if (talk.message_flags.is == "1")
          simple_message_tag(useractioner.handle_name.is + " 移除同生共死" , true, "hunter-do")
        else
          simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用血之哀傷" , true, "hunter-do")
      case MTypeEnum.ACTION_MIKO_REVERSEBLEED      =>
        val message = 
          if (reveal_mode || (useractioner.id.is == currentuserentry_id))
            "(" + talk.message_flags.is.split(",").toList.map(x => CardEnum.get_card(x).card_name).mkString(",") + ")"
          else
            "(" + talk.message_flags.is.split(",").toList.length + "張)"
        simple_message_tag(useractioner.handle_name.is + " 使用逆流" + message , true, "hunter-do")
      case MTypeEnum.ACTION_MIKO_BLOODCURSE        =>
        val message = 
          if (reveal_mode || (useractioner.id.is == currentuserentry_id))
            "(" + talk.message_flags.is.split(",").toList.map(x => CardEnum.get_card(x).card_name).mkString(",") + ")"
          else
            "(" + talk.message_flags.is.split(",").toList.length + "張)"
        simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用血之詛咒" + message , true, "hunter-do")
      case MTypeEnum.ACTION_BUTTERFLY_DANCE        =>
        val message = 
          if (talk.message_flags.is == "")
            "(摸1張牌)"
          else if (reveal_mode || (useractioner.id.is == currentuserentry_id))
            "(" + CardEnum.get_card(talk.message_flags.is).card_name + ")"
          else
            "(棄1張牌)"
        simple_message_tag(useractioner.handle_name.is + " 使用舞動" + message, true, "hunter-do")
      case MTypeEnum.ACTION_BUTTERFLY_PILGRIMAGE   =>
        val message = 
          if (talk.message_flags2.is != "0")
            "(" + CardEnum.get_card(talk.message_flags.is).card_name + "(凋零:" + 
              UserEntry.get(PlummUtil.parseLong(talk.message_flags2.is), userentrys).handle_name.is + "))"
          else if (reveal_mode || (useractioner.id.is == currentuserentry_id))
            "(" + CardEnum.get_card(talk.message_flags.is).card_name + ")"
          else
            "(1張)"
        simple_message_tag(useractioner.handle_name.is + " 使用朝聖" + message , true, "hunter-do")
      case MTypeEnum.ACTION_BUTTERFLY_POISONPOWDER =>
        val message = 
          if (talk.message_flags2.is != "0")
            "(" + CardEnum.get_card(talk.message_flags.is).card_name + "(凋零:" + 
              UserEntry.get(PlummUtil.parseLong(talk.message_flags2.is), userentrys).handle_name.is + "))"
          else if (reveal_mode || (useractioner.id.is == currentuserentry_id))
            "(" + CardEnum.get_card(talk.message_flags.is).card_name + ")"
          else
            "(1張)"
        simple_message_tag(useractioner.handle_name.is + " 使用毒粉" + message, true, "hunter-do")
      case MTypeEnum.ACTION_BUTTERFLY_MIRRORFLOWER =>
        val message_flags1 = talk.message_flags.is.split(",")
        val message_flags2 = talk.message_flags2.is.split(",")
        def wither_message(flag1 : String, flag2 : String) = 
          if (flag2 != "0")
             CardEnum.get_card(flag1).card_name + "(凋零:" + 
               UserEntry.get(PlummUtil.parseLong(flag2), userentrys).handle_name.is + ")"
          else 
             CardEnum.get_card(flag1).card_name 
        simple_message_tag(useractioner.handle_name.is + " 使用鏡花水月(" 
            + wither_message(message_flags1(0), message_flags2(0)) + "," + wither_message(message_flags1(1), message_flags2(1)) +")", true, "hunter-do")
      case MTypeEnum.ACTION_BUTTERFLY_BACKDISCARD  =>
        val message = 
          if (reveal_mode || (useractioner.id.is == currentuserentry_id))
            "(" + talk.message_flags.is.split(",").toList.map(x => CardEnum.get_card(x).card_name).mkString(",") + ")"
          else
            "(" + talk.message_flags.is.split(",").toList.length + "張)"
        simple_message_tag(useractioner.handle_name.is + " 丟棄蓋牌" + message, true, "hunter-do")
      case MTypeEnum.ACTION_BUTTERFLY_COCOON       =>
        simple_message_tag(useractioner.handle_name.is + " 使用蛹化" , true, "hunter-do")
      case MTypeEnum.ACTION_BUTTERFLY_REVERSEFLY   =>
        val message_flags1 = talk.message_flags.is.split(",")
        val message_flags2 = talk.message_flags2.is.split(",")
        val message = 
          if (reveal_mode || (useractioner.id.is == currentuserentry_id))
            "(" + message_flags1.take(2).map(x => CardEnum.get_card(x).card_name).mkString(",") + ")"
          else
            "(" + message_flags1.take(2).length + "張)"
        if (talk.actionee_id.is != 0) {
          simple_message_tag(useractioner.handle_name.is + " 對 " + useractionee.handle_name.is + " 使用倒逆之蝶(法術傷害)" + message, true, "hunter-do")
        } else if (message_flags1.length == 4) {
          def wither_message(flag1 : String, flag2 : String) = 
            if (flag2 != "0")
               CardEnum.get_card(flag1).card_name + "(凋零:" + 
                 UserEntry.get(PlummUtil.parseLong(flag2), userentrys).handle_name.is + ")"
            else if (reveal_mode || (useractioner.id.is == currentuserentry_id))
               CardEnum.get_card(flag1).card_name 
            else
              "1張"
          simple_message_tag(useractioner.handle_name.is + " 使用倒逆之蝶(棄繭移蛹)" + message +
            "(" + wither_message(message_flags1(2), message_flags2(0)) + "," + wither_message(message_flags1(3), message_flags2(1)) + ")", true, "hunter-do")
        } else
          simple_message_tag(useractioner.handle_name.is + " 使用倒逆之蝶(自傷移蛹)" + message , true, "hunter-do")
        
      case xs => NodeSeq.Empty
    }
  }
  
  // Message Table
  def messages_normal(room: Room, roomround : RoomRound, userentrys: List[UserEntry], reveal:Boolean) : NodeSeq = {
    //val roomround = RoomRound_R.get
    var talks =  Talk.findAll(By(Talk.roomround_id, roomround.id.is), OrderBy(Talk.id, Descending))
    
    val lastround_id = roomround.last_round.is
    if (lastround_id != 0)
      talks = talks ++ Talk.findAll(By(Talk.roomround_id, lastround_id), OrderBy(Talk.id, Descending))
    
    if (roomround.round_no.is == 0) {
      val revotes = talks.filter(_.mtype.is == MTypeEnum.MESSAGE_REVOTE0.toString)

      // 新增 投票重新開始 50 次時廢村
      if ((revotes.length >= 50) || (talks.length >= 1000)) {
        //val room = Room_R.get
        /*
        room.status(RoomStatusEnum.ENDED.toString).victory(RoomVictoryEnum.ABANDONED.toString)
        room.save
            
        RoomActor ! SessionVarSet(room = room)
        RoomActor.sendRoomMessage(room.id.is, RoomForceUpdate(room.id.is ,List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.ACTION_BAR)))
        */
       GameProcessor.abandon(room)
      }
    }
    
    // border="0" cellpadding="0" cellspacing="0"
    Seq(<table class="talk"> <tbody id="talk-tbody">{
        for (talk <- talks) yield talk_tag(talk, userentrys, reveal)
      } </tbody></table>)
  }
  
  //val amountRange = Expense.findAll(   BySql("amount between ? and ?", lowVal, highVal))
  
  def messages_all(room_id : Long, userentrys: List[UserEntry], reveal:Boolean) : NodeSeq = {
    val talks = Talk.findAll(In(Talk.roomround_id, RoomRound.id, By(RoomRound.room_id, room_id)), OrderBy(Talk.id, Descending))
    //val talks        =  Talk.findAllByPreparedStatement({ superconn =>
     // val statement = superconn.connection.prepareStatement(
     // "select * from Talk join RoomRound on Talk.roomround_id = RoomRound.id where RoomRound.room_id = ? order by Talk.id desc")
     // statement.setString(1, room_id.toString)
     // statement
    //})
  
    warn("messages_all : " + userentrys.toString)
    
    // border="0" cellpadding="0" cellspacing="0"
    Seq(<table class="talk"> <tbody id="talk-tbody">{
        for (talk <- talks) yield talk_tag(talk, userentrys, reveal)
      } </tbody></table>)
  }
}


/*
<tr>
<td class="system-user" colspan="2"><img src="img/icon/hum.gif"> clojure 來到了幻想鄉</td>
</tr>
<tr>
<td class="system-user" colspan="2"><img src="img/icon/msg.gif"> ＜投票結果重置，請重新投票＞</td>
</tr>
<tr>
<td class="system-user" colspan="2"><img src="img/icon/spy.gif"> 映 被踢出村莊了</td>
</tr>
<tr class="system-message">
<td class="kick-do" colspan="2">映 對 映 投票踢出</td>
</tr>
<tr class="system-message">
<td class="kick-do" colspan="2">映 對 右代宮  戰人 投票踢出</td>
</tr>
<tr>
<td class="system-user" colspan="2"><img src="img/icon/msg.gif"> ＜投票結果重置，請重新投票＞</td>
</tr>
<tr>
<td class="system-user" colspan="2"><img src="img/icon/spy.gif"> 天晴 被踢出村莊了</td>
</tr>
<tr class="system-message">
<td class="kick-do" colspan="2">天晴 對 天晴 投票踢出</td>
</tr>
<tr class="user-talk">
<td class="user-name"><font style="color:#00DD77">◆</font>天晴</td>
<td class="say normal">「鑽石開始了 我先自刪 掰」</td>
</tr> */