package org.plummtw.astgrail.enum

import org.plummtw.astgrail.model.{UserEntry, RoomPhase, GameObject}

object RoomPhaseEnum extends Enumeration {
  type RoomPhaseEnum = Value
  
  val NONE             = Value("")
  
  val GAMEHALL        = Value("GH")
  val DRAFT            = Value("DR")
  
  val ACTIVATE         = Value("AC")
  val MAIN             = Value("MA")
  val MAIN_NO_ACTIVATE = Value("MA2")
  
  val ATTACK          = Value("AT")
  val REATTACK        = Value("RAT")
  val ATTACK_REACTION = Value("R_AT")
  val HEAL_REACTION    = Value("R_HE")
  
  val ATTACK_OR_MAGIC = Value("ATM")
  val MAGIC           = Value("MAG")
  val MBOLT_REACTION  = Value("R_MB")
  val WEAKEN_REACTION = Value("R_WE")
  
  val DISCARD_REACTION = Value("R_DC")

  val ENDED            = Value("EN")
  
  val PROCESS            = Value("P_")
  val PROCESS_DAMAGE   = Value("P_DA")
  val PROCESS_POISON   = Value("P_PO")
  val PROCESS_MORALHIT  = Value("P_MI")
  val DELAYED_ACTION    = Value("P_DAC")
  
  val ADDITIONALTURN_REACTION  = Value("R_AD")
  val LACERATE_REACTION        = Value("R_BE3")
  val PENETRATE_REACTION       = Value("R_AR3")
  val WATERSHADOW_REACTION     = Value("R_AS1")
  val FIVESEAL_REACTION        = Value("R_SE2")
  val SHOCK_REACTION           = Value("R_MA1")
  val ANGELBLESS_REACTION      = Value("R_AN3")
  val ANGELSONG_REACTION      = Value("R_AN5")
  val GODCOVER_REACTION        = Value("R_AN6")
  val SMITE_REACTION           = Value("R_SL2")
  val REFLECT_REACTION         = Value("R_SA1")
  val AIRRUNE_REACTION         = Value("R_RM2")
  val GHOSTS100_REACTION       = Value("R_RM3")
  
  val POWERBLESS_REACTION      = Value("R_PR1")
  val FASTBLESS_REACTION       = Value("R_PR2")
  val MANATIDE_REACTION        = Value("R_PR6")
  val SWORDKI_REACTION         = Value("R_SE1")  
  val LIBIDINALWILL_REACTION   = Value("R_SE4")
  val KISHOT_REACTION          = Value("R_MO2")
  //val TAUNT_REACTION           = Value("R_BR2")
  val FORBIDDEN_REACTION      = Value("R_BR4")
  val DEATHMATCH_REACTION     = Value("R_BR5")
  val BLOODFEAST_REACTION     = Value("R_RK2")
  val DISCIPLINE_REACTION     = Value("R_RK3")
  val SOULLINK_REACTION       = Value("R_SM6")
  val POISONPOWDER_REACTION   = Value("R_BU2")
  val PILGRIMAGE_REACTION     = Value("R_BU3")
  val MIRRORFLOWER_REACTION   = Value("R_BU4")
  val BACKDISCARD_REACTION    = Value("R_BU5")
  
  
  val CNAME_MAP     = scala.collection.immutable.TreeMap(
    NONE              -> "無",
    GAMEHALL          -> "大廳行動",
    DRAFT             -> "選擇角色",
    ACTIVATE          -> "啟動行動",
    MAIN              -> "主要行動",
    MAIN_NO_ACTIVATE  -> "攻擊或魔法",
    ATTACK            -> "攻擊行動",
    MAGIC             -> "魔法行動",
    ATTACK_OR_MAGIC   -> "攻擊或魔法",
    ATTACK_REACTION   -> "應戰行動",
    HEAL_REACTION     -> "治療回應",
    MBOLT_REACTION    -> "魔彈回應",
    WEAKEN_REACTION   -> "虛弱回應",
    
    DISCARD_REACTION        -> "棄牌行動",
    ADDITIONALTURN_REACTION -> "額外行動回應",
    LACERATE_REACTION       -> "撕裂回應",
    PENETRATE_REACTION      -> "貫穿射擊回應",
    WATERSHADOW_REACTION  -> "水影回應",
    ANGELBLESS_REACTION     -> "天使祝福回應",
    ANGELSONG_REACTION     -> "天使之歌回應",
    GODCOVER_REACTION      -> "神之庇護回應",
    FIVESEAL_REACTION       -> "五系束縛回應",
    SHOCK_REACTION          -> "魔爆衝擊回應",
    SMITE_REACTION          -> "聖擊地槍回應",
    REFLECT_REACTION        -> "法術反彈回應",
    AIRRUNE_REACTION        -> "靈符風行棄牌回應",
    GHOSTS100_REACTION      -> "百鬼夜行回應",
    POWERBLESS_REACTION     -> "威力賜福回應",
    FASTBLESS_REACTION      -> "迅捷賜福回應",
    MANATIDE_REACTION       -> "法力潮汐回應",
    SWORDKI_REACTION        -> "劍氣斬回應",
    LIBIDINALWILL_REACTION  -> "不屈意志回應",
    KISHOT_REACTION         -> "氣彈回應",
    FORBIDDEN_REACTION      -> "禁斷之力回應",
    DEATHMATCH_REACTION    -> "死鬥回應",
    BLOODFEAST_REACTION     -> "殺戮盛宴回應",
    DISCIPLINE_REACTION     ->  "戒驕戒躁回應",
    SOULLINK_REACTION       ->  "傷害轉移回應",
    POISONPOWDER_REACTION   ->  "毒粉回應",
    PILGRIMAGE_REACTION     ->  "朝聖回應",
    MIRRORFLOWER_REACTION   ->  "鏡花水月回應",
    BACKDISCARD_REACTION    ->  "蓋牌棄牌行動",
    
    ENDED             -> "結束行動"
  )
  
  def get_cname(phase : RoomPhaseEnum.Value) : String =
    CNAME_MAP.get(phase).getOrElse("BUG : " + phase.toString)
  
  def get_cname(roomphase : RoomPhase, userentrys : List[UserEntry]) : String = {
    val phase_type = roomphase.phase_type.is
    val result = 
      get_cname(try {RoomPhaseEnum.withName(phase_type)}
        catch {case e : Exception => NONE})
    if ((phase_type == RoomPhaseEnum.POISONPOWDER_REACTION.toString) ||
        (phase_type == RoomPhaseEnum.MIRRORFLOWER_REACTION.toString))
      result + "(" + UserEntry.get(roomphase.actionee_id.is, userentrys).handle_name + " 對 " +
                     UserEntry.get(roomphase.actionee2_id.is, userentrys).handle_name + " 的魔法傷害)"
    else result
  }

  def get(phase : String) : RoomPhaseEnum.Value =
    try {RoomPhaseEnum.withName(phase)}
      catch {case e : Exception => NONE}
  
  implicit def gamephaseenum2String (en : RoomPhaseEnum.Value) : String = en.toString
}

object RoomAdditionalFlagEnum extends Enumeration {
  type RoomAdditionalFlagEnum = Value
  
  val NONE             = Value("")
  
  val ATTACK           = Value("A")
  val MAGIC            = Value("M")
  val BOTH             = Value("B")
  val ATTACK_AIR       = Value("I")
  val ATTACK_FIRE      = Value("F")
  val ACTIVATE         = Value("C")
  
  val DONE             = Value("D")
}