package org.plummtw.astgrail.data

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
import org.plummtw.astgrail.card._

class RoleData(val enum : RoleEnum.Value, val name : String, val level : Int,
                val subattr : CardSubAttrEnum.Value,
                val is_expand : Boolean = false, val is_specific : Boolean = false) extends Logger {
  def role_enum = enum
  def role_name = name
  def role_level= level
  def role_subattr = subattr
  
  def role_is_expand = is_expand
  def role_is_specific = is_specific
  //def role_side = side
  
  val role_use_tap           = false
  val role_back_cards_max   = 0
  val role_yellow_index_max = 0
  val role_blue_index_max   = 0
  val role_is_gem_only         = false
  
  def cfield(cssclass : String) = {
    <span class={cssclass}>[{role_name}]</span>
  }
  
  //def simple_cfield = {
  //  <span class={RoleSideEnum.get_roleside_color(role_side)}>[{role_name.substring(0,1)}]</span>
  //}
  
  /*
  def movement_skill : ActionData = ActionNoAction
  def attack_skill   : ActionData = ActionNoAction
  def free_skill     : ActionData = ActionNoAction
  def post_skill     : ActionData = ActionNoAction
  */
 
  val skills_hash = scala.collection.immutable.TreeMap(
    RoomPhaseEnum.MAIN                  -> List(ActionAttack, ActionMagicPoison, ActionMagicWeaken,
                                                ActionMagicShield, ActionMagicMBolt,
                                                ActionPurchase, ActionCombine, ActionRefine),
    RoomPhaseEnum.MAIN_NO_ACTIVATE      -> List(ActionAttack, ActionMagicPoison, ActionMagicWeaken,
                                                ActionMagicShield, ActionMagicMBolt),
    RoomPhaseEnum.ATTACK                -> List(ActionAttack, ActionNoAction),
    RoomPhaseEnum.MAGIC                 -> List(ActionMagicPoison, ActionMagicWeaken,
                                                ActionMagicShield, ActionMagicMBolt, ActionNoAction),
    //RoomPhaseEnum.ATTACK_OR_MAGIC       -> List()
    RoomPhaseEnum.ATTACK_REACTION       -> List(ActionReattack, ActionLight, ActionEndureAttack),
    RoomPhaseEnum.HEAL_REACTION         -> List(ActionHeal),

    RoomPhaseEnum.MBOLT_REACTION        -> List(ActionMagicMBolt, ActionLight, ActionEndureMagic),
    
    RoomPhaseEnum.WEAKEN_REACTION       -> List(ActionSkipTurn, ActionEndureMagic),
    RoomPhaseEnum.FIVESEAL_REACTION     -> List(ActionSkipTurn, ActionEndureMagic),
    
    RoomPhaseEnum.SHOCK_REACTION        -> List(ActionMageShockDiscard,  ActionNoAction),
    RoomPhaseEnum.ANGELBLESS_REACTION   -> List(ActionAngelAngelBlessGive),
    RoomPhaseEnum.ANGELSONG_REACTION     -> List(ActionAngelAngelSong, ActionNoAction),
    RoomPhaseEnum.AIRRUNE_REACTION       -> List(ActionRuneMageAirRuneDiscard),
    
    RoomPhaseEnum.POWERBLESS_REACTION    -> List(ActionPrayerPowerBlessUse, ActionNoAction),
    RoomPhaseEnum.FASTBLESS_REACTION     -> List(ActionPrayerFastBlessUse, ActionNoAction),
    //RoomPhaseEnum.MANATIDE_REACTION      -> List(ActionPrayerManaTide,    ActionNoAction),
    
    RoomPhaseEnum.DISCARD_REACTION      -> List(ActionDiscard)
  )
  
  val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
    scala.collection.immutable.TreeMap()
 
  def skills(roomphaseenum : RoomPhaseEnum.Value)  : List[ActionData]= {
    warn("skills : " + roomphaseenum.toString)
    if (roomphaseenum == RoomPhaseEnum.ATTACK_OR_MAGIC) {
      skills(RoomPhaseEnum.MAIN_NO_ACTIVATE) ::: List(ActionNoAction)
    } else {
      val role_skills_list = role_skills_hash.get(roomphaseenum) match {
        case Some(x) => x
        case xs      => List()
      }
      val basic_skills_list = skills_hash.get(roomphaseenum) match {
        case Some(y) => y
        case ys => List()
      }
      basic_skills_list ::: role_skills_list
    }
  }
  
  def role_skills(roomphaseenum : RoomPhaseEnum.Value)  : List[ActionData]= {
    warn("skills : " + roomphaseenum.toString)
    val role_skills_list = role_skills_hash.get(roomphaseenum) match {
      case Some(x) => x
      case xs      => List()
    }
    role_skills_list
  }
    
}

object RoleNone extends RoleData(RoleEnum.NONE, "不指定", 6, CardSubAttrEnum.NONE) 

object RoleArcher extends RoleData(RoleEnum.ARCHER, "弓之女神", 6, CardSubAttrEnum.SKILL) {
  override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
    scala.collection.immutable.TreeMap(
      RoomPhaseEnum.MAIN             -> List(ActionArcherTrap, ActionArcherSnipe),
      RoomPhaseEnum.MAIN_NO_ACTIVATE -> List(ActionArcherTrap, ActionArcherSnipe),
      RoomPhaseEnum.MAGIC            -> List(ActionArcherTrap, ActionArcherSnipe),
      RoomPhaseEnum.PENETRATE_REACTION -> List(ActionArcherPenetrate, ActionNoAction)
    )
  
}
object RoleAssassin extends RoleData(RoleEnum.ASSASSIN, "暗殺者", 6, CardSubAttrEnum.SKILL) {
   override val role_use_tap = true
   override val role_is_gem_only = true
   override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
    scala.collection.immutable.TreeMap(
      RoomPhaseEnum.MAIN                       -> List(ActionAssassinSneak),
      RoomPhaseEnum.WATERSHADOW_REACTION       -> List(ActionAssassinWaterShadow, ActionNoAction)
    )
}
object RoleSwordSaint extends RoleData(RoleEnum.SWORDSAINT, "劍聖", 6, CardSubAttrEnum.SKILL) {
  override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
    scala.collection.immutable.TreeMap(
      RoomPhaseEnum.ADDITIONALTURN_REACTION       -> List(ActionSwordSaintCombo, ActionSwordSaintSwordShadow, ActionNoAction)
    )
}
object RoleAngel extends RoleData(RoleEnum.ANGEL, "天使", 6, CardSubAttrEnum.HOLY) {
  override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
    scala.collection.immutable.TreeMap(
      RoomPhaseEnum.GODCOVER_REACTION -> List(ActionAngelGodCover, ActionNoAction),
      RoomPhaseEnum.MAIN                -> List(ActionAngelWindPurify, ActionAngelAngelBless),
      RoomPhaseEnum.MAIN_NO_ACTIVATE   -> List(ActionAngelWindPurify, ActionAngelAngelBless),
      RoomPhaseEnum.MAGIC               -> List(ActionAngelWindPurify, ActionAngelAngelBless)
    )
}
object RoleSaintGirl extends RoleData(RoleEnum.SAINTGIRL, "聖女", 6, CardSubAttrEnum.HOLY) {
   override val role_use_tap = true
   override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
    scala.collection.immutable.TreeMap(
      RoomPhaseEnum.MAIN             -> List(ActionSaintGirlHeal, ActionSaintGirlHealLight, ActionSaintGirlCompassion, ActionSaintGirlHolyHeal),
      RoomPhaseEnum.MAIN_NO_ACTIVATE -> List(ActionSaintGirlHeal, ActionSaintGirlHealLight, ActionSaintGirlHolyHeal),
      RoomPhaseEnum.MAGIC            -> List(ActionSaintGirlHeal, ActionSaintGirlHealLight, ActionSaintGirlHolyHeal)
    )
}
object RoleBerserker extends RoleData(RoleEnum.BERSERKER, "狂戰士", 6, CardSubAttrEnum.BLOOD) {
  override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
    scala.collection.immutable.TreeMap(
      RoomPhaseEnum.LACERATE_REACTION       -> List(ActionBerserkerLacerate, ActionNoAction)
    )
  
  override val role_is_gem_only         = true
}
object RoleSealer extends RoleData(RoleEnum.SEALER, "封印師", 6, CardSubAttrEnum.ILLUSION, is_specific = true)  {
  override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] = 
    scala.collection.immutable.TreeMap(
      RoomPhaseEnum.MAIN             -> List(ActionSealerSeal, ActionSealerFiveSeal, ActionSealerBreakSeal),
      RoomPhaseEnum.MAIN_NO_ACTIVATE -> List(ActionSealerSeal, ActionSealerFiveSeal, ActionSealerBreakSeal),
      RoomPhaseEnum.MAGIC            -> List(ActionSealerSeal, ActionSealerFiveSeal, ActionSealerBreakSeal)
    )
}  
object RoleMage extends RoleData(RoleEnum.MAGE, "魔導師", 6, CardSubAttrEnum.CAST) {
  override val role_is_gem_only = true
  override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
    scala.collection.immutable.TreeMap(
      RoomPhaseEnum.MAIN                -> List(ActionMageShock, ActionMageStorm),
      RoomPhaseEnum.MAIN_NO_ACTIVATE   -> List(ActionMageShock, ActionMageStorm),
      RoomPhaseEnum.MAGIC               -> List(ActionMageShock, ActionMageStorm)
    )
}
  
object RoleJudicator extends RoleData(RoleEnum.JUDICATOR, "仲裁者", 7, CardSubAttrEnum.BLOOD, is_expand = true) {
   override val role_use_tap = true
   override val role_yellow_index_max = 4
   override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
    scala.collection.immutable.TreeMap(
      RoomPhaseEnum.MAIN                 -> List(ActionJudicatorBreakRitual, ActionJudicatorFinalJudge, ActionJudicatorRitual, ActionJudicatorBalance),
      RoomPhaseEnum.MAIN_NO_ACTIVATE     -> List(ActionJudicatorFinalJudge,  ActionJudicatorBalance),
      RoomPhaseEnum.MAGIC                -> List(ActionJudicatorFinalJudge,  ActionJudicatorBalance)
    )
}
object RoleMagicSword extends RoleData(RoleEnum.MAGICSWORD, "魔劍", 7, CardSubAttrEnum.ILLUSION) {
   override val role_use_tap = true
   override val role_is_gem_only = true
   override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
    scala.collection.immutable.TreeMap(
      RoomPhaseEnum.MAIN                 -> List(ActionMagicSwordGather),
      RoomPhaseEnum.MAIN_NO_ACTIVATE     -> List(ActionMagicSwordComet),
      RoomPhaseEnum.MAGIC                -> List(ActionMagicSwordComet)
    )
}
object RoleNecromancer extends RoleData(RoleEnum.NECROMANCER, "死靈法師", 7, CardSubAttrEnum.ILLUSION) {
  override val role_is_gem_only = true
  override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
    scala.collection.immutable.TreeMap(
      RoomPhaseEnum.MAIN                 -> List(ActionNecromancerPlague, ActionNecromancerDeathTouch, ActionNecromancerGraveFall),
      RoomPhaseEnum.MAIN_NO_ACTIVATE     -> List(ActionNecromancerPlague, ActionNecromancerDeathTouch, ActionNecromancerGraveFall),
      RoomPhaseEnum.MAGIC                -> List(ActionNecromancerPlague, ActionNecromancerDeathTouch, ActionNecromancerGraveFall)
    )
}
object RoleAdventurer extends RoleData(RoleEnum.ADVENTURER, "冒險家", 7, CardSubAttrEnum.ILLUSION) {
  override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
    scala.collection.immutable.TreeMap(
      RoomPhaseEnum.MAIN                 -> List(ActionAdventurerDeceive, ActionAdventurerAddOn, ActionAdventurerTheft),
      RoomPhaseEnum.MAIN_NO_ACTIVATE     -> List(ActionAdventurerDeceive, ActionAdventurerAddOn, ActionAdventurerTheft),
      RoomPhaseEnum.ATTACK               -> List(ActionAdventurerDeceive),
      RoomPhaseEnum.MAGIC                -> List(ActionAdventurerAddOn, ActionAdventurerTheft)
    )
  
}
object RoleElementalist extends RoleData(RoleEnum.ELEMENTALIST, "元素師", 7, CardSubAttrEnum.CAST) {
  override val role_is_gem_only = true
  override val role_yellow_index_max = 3
  override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
    scala.collection.immutable.TreeMap(
      RoomPhaseEnum.MAIN                 -> List(ActionElementalistMagic, ActionElementalistIgnite, ActionElementalistMoonLight),
      RoomPhaseEnum.MAIN_NO_ACTIVATE     -> List(ActionElementalistMagic, ActionElementalistIgnite, ActionElementalistMoonLight),
      RoomPhaseEnum.MAGIC                -> List(ActionElementalistMagic, ActionElementalistIgnite, ActionElementalistMoonLight)
    )
}
object RoleSaintLance extends RoleData(RoleEnum.SAINTLANCE, "聖槍", 7, CardSubAttrEnum.HOLY, is_expand = true) {
  override val role_is_gem_only = true
  override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
    scala.collection.immutable.TreeMap(
      RoomPhaseEnum.SMITE_REACTION       -> List(ActionSaintLanceSmite, ActionSaintLanceEarthLance, ActionNoAction),
      RoomPhaseEnum.MAIN                 -> List(ActionSaintLanceShine, ActionSaintLanceRetribution, ActionSaintLanceHolyPray),
      RoomPhaseEnum.MAIN_NO_ACTIVATE     -> List(ActionSaintLanceShine,ActionSaintLanceRetribution, ActionSaintLanceHolyPray),
      RoomPhaseEnum.MAGIC                -> List(ActionSaintLanceShine,ActionSaintLanceRetribution, ActionSaintLanceHolyPray)
    )
}


object RoleRuneMage extends RoleData(RoleEnum.RUNEMAGE, "靈符師", 8, CardSubAttrEnum.CAST, is_expand = true) {
   override val role_back_cards_max   = 2
    override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
    scala.collection.immutable.TreeMap(
      RoomPhaseEnum.GHOSTS100_REACTION   -> List(ActionRuneMage100Ghosts, ActionNoAction),
      RoomPhaseEnum.MAIN                 -> List(ActionRuneMageThunderRune, ActionRuneMageAirRune),
      RoomPhaseEnum.MAIN_NO_ACTIVATE     -> List(ActionRuneMageThunderRune, ActionRuneMageAirRune),
      RoomPhaseEnum.MAGIC                -> List(ActionRuneMageThunderRune, ActionRuneMageAirRune)
    )
}
object RoleBishop   extends RoleData(RoleEnum.BISHOP, "神官", 8, CardSubAttrEnum.HOLY, is_expand = true) {
  override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
    scala.collection.immutable.TreeMap(
      RoomPhaseEnum.MAIN                 -> List(ActionBishopHolyBless, ActionBishopHolyWater, ActionBishopHolyContract, ActionBishopHolyField),
      RoomPhaseEnum.MAIN_NO_ACTIVATE     -> List(ActionBishopHolyBless, ActionBishopHolyWater, ActionBishopHolyField),
      RoomPhaseEnum.MAGIC                -> List(ActionBishopHolyBless, ActionBishopHolyWater, ActionBishopHolyField)
    )
  
}
object RolePrayer    extends RoleData(RoleEnum.PRAYER, "祈禱師", 8, CardSubAttrEnum.CAST) {
  override val role_use_tap = true
  override val role_yellow_index_max = 3
  override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
    scala.collection.immutable.TreeMap(
      RoomPhaseEnum.MANATIDE_REACTION    -> List(ActionPrayerManaTide, ActionNoAction),
      RoomPhaseEnum.MAIN                 -> List(ActionPrayerShineBelieve, ActionPrayerDarkBelieve, ActionPrayerPowerBless, ActionPrayerFastBless, ActionPrayerPray),
      RoomPhaseEnum.MAIN_NO_ACTIVATE     -> List(ActionPrayerShineBelieve, ActionPrayerDarkBelieve, ActionPrayerPowerBless, ActionPrayerFastBless),
      RoomPhaseEnum.MAGIC                -> List(ActionPrayerShineBelieve, ActionPrayerDarkBelieve, ActionPrayerPowerBless, ActionPrayerFastBless)
    )
  
  
}
object RoleSage      extends RoleData(RoleEnum.SAGE, "賢者", 8, CardSubAttrEnum.CAST) {
  override val role_is_gem_only = true
  override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
    scala.collection.immutable.TreeMap(
      RoomPhaseEnum.REFLECT_REACTION     -> List(ActionSageReflect, ActionNoAction),
      RoomPhaseEnum.MAIN                 -> List(ActionSageMagicBook, ActionSageHolyBook),
      RoomPhaseEnum.MAIN_NO_ACTIVATE     -> List(ActionSageMagicBook, ActionSageHolyBook),
      RoomPhaseEnum.MAGIC                -> List(ActionSageMagicBook, ActionSageHolyBook)
    )
}

object RoleRedKnight      extends RoleData(RoleEnum.REDKNIGHT, "紅蓮騎士", 8, CardSubAttrEnum.BLOOD) {
  override val role_use_tap = true
  override val role_yellow_index_max = 2
  override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
    scala.collection.immutable.TreeMap(
      RoomPhaseEnum.BLOODFEAST_REACTION  -> List(ActionRedKnightBloodFeast, ActionNoAction),
      RoomPhaseEnum.DISCIPLINE_REACTION  -> List(ActionRedKnightDiscipline, ActionNoAction),
      RoomPhaseEnum.MAIN                 -> List(ActionRedKnightBloodPray,  ActionRedKnightBloodCross),
      RoomPhaseEnum.MAIN_NO_ACTIVATE     -> List(ActionRedKnightBloodCross),
      RoomPhaseEnum.MAGIC                -> List(ActionRedKnightBloodCross)
    )
}

object RoleSwordEmp extends RoleData(RoleEnum.SWORDEMP, "劍帝", 9, CardSubAttrEnum.SKILL, is_expand = true) {
   override val role_back_cards_max   = 3
   override val role_yellow_index_max = 5
   override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
    scala.collection.immutable.TreeMap(
      RoomPhaseEnum.SWORDKI_REACTION        -> List(ActionSwordEmpSwordKi, ActionNoAction),
      RoomPhaseEnum.LIBIDINALWILL_REACTION  -> List(ActionSwordEmpLibidinalWill, ActionNoAction)
    )
}
object RoleMonk     extends RoleData(RoleEnum.MONK, "格鬥家", 9, CardSubAttrEnum.SKILL, is_expand = true) {
   override val role_use_tap = true
   override val role_yellow_index_max = 6
   override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
    scala.collection.immutable.TreeMap(
      RoomPhaseEnum.KISHOT_REACTION      -> List(ActionMonkKiShot, ActionNoAction),
      RoomPhaseEnum.MAIN                 -> List(ActionMonk100Dragons, ActionMonkMonkGod)
    )
}
object RoleBrave    extends RoleData(RoleEnum.BRAVE, "勇者", 9, CardSubAttrEnum.BLOOD, is_expand = true, is_specific = true) {
   override val role_use_tap = true
   override val role_yellow_index_max = 4
   override val role_blue_index_max = 4
   override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
     scala.collection.immutable.TreeMap(
       RoomPhaseEnum.FORBIDDEN_REACTION  -> List(ActionBraveForbidden, ActionNoAction),
       RoomPhaseEnum.DEATHMATCH_REACTION  -> List(ActionBraveDeathMatch, ActionNoAction),
       RoomPhaseEnum.MAIN                 -> List(ActionBraveTaunt),
       RoomPhaseEnum.MAIN_NO_ACTIVATE     -> List(ActionBraveTaunt),
       RoomPhaseEnum.MAGIC                -> List(ActionBraveTaunt)
    )
}
object RoleSoulMage extends RoleData(RoleEnum.SOULMAGE, "靈魂術士", 9, CardSubAttrEnum.ILLUSION, is_expand = true, is_specific = true) {
   override val role_yellow_index_max = 6
   override val role_blue_index_max = 6
   override val role_is_gem_only = true
   override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
    scala.collection.immutable.TreeMap(
      RoomPhaseEnum.SOULLINK_REACTION  -> List(ActionSoulMageSoulLinkTransfer, ActionNoAction),
      RoomPhaseEnum.MAIN                 -> List(ActionSoulMageSoulGive, ActionSoulMageSoulBurst, ActionSoulMageSoulSummon, ActionSoulMageSoulMirror, ActionSoulMageSoulLink, ActionSoulMageSoulEnhance),
      RoomPhaseEnum.MAIN_NO_ACTIVATE     -> List(ActionSoulMageSoulGive, ActionSoulMageSoulBurst, ActionSoulMageSoulSummon, ActionSoulMageSoulMirror),
      RoomPhaseEnum.MAGIC                -> List(ActionSoulMageSoulGive, ActionSoulMageSoulBurst, ActionSoulMageSoulSummon, ActionSoulMageSoulMirror)
    )
}
  
object RoleMiko       extends RoleData(RoleEnum.MIKO, "巫女", 10, CardSubAttrEnum.CAST, is_expand = true) {
   override val role_use_tap = true
   override val role_is_gem_only = true
   override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
    scala.collection.immutable.TreeMap(
      RoomPhaseEnum.MAIN                 -> List(ActionMikoBloodCry, ActionMikoStickWith, ActionMikoBloodSorrow, ActionMikoReverseBleed, ActionMikoBloodCurse),
      RoomPhaseEnum.MAIN_NO_ACTIVATE     -> List(ActionMikoBloodCry, ActionMikoStickWith, ActionMikoReverseBleed, ActionMikoBloodCurse),
      RoomPhaseEnum.MAGIC                -> List(ActionMikoBloodCry, ActionMikoStickWith, ActionMikoReverseBleed, ActionMikoBloodCurse)
    )
}
object RoleButterfly extends RoleData(RoleEnum.BUTTERFLY, "蝶舞者", 10, CardSubAttrEnum.BLOOD, is_expand = true, is_specific = true) {
   override val role_back_cards_max   = 8
   override val role_yellow_index_max = 9999
   override val role_skills_hash : scala.collection.immutable.TreeMap[RoomPhaseEnum.Value, List[ActionData]] =
    scala.collection.immutable.TreeMap(
      RoomPhaseEnum.POISONPOWDER_REACTION -> List(ActionButterflyPoisonPowder, ActionNoAction),
      RoomPhaseEnum.PILGRIMAGE_REACTION   -> List(ActionButterflyPilgrimage, ActionNoAction),
      RoomPhaseEnum.MIRRORFLOWER_REACTION -> List(ActionButterflyMirrorFlower, ActionNoAction),
      RoomPhaseEnum.BACKDISCARD_REACTION  -> List(ActionButterflyBackDiscard),
      RoomPhaseEnum.MAIN                 -> List(ActionButterflyDance, ActionButterflyCocoon, ActionButterflyReverseFly),
      RoomPhaseEnum.MAIN_NO_ACTIVATE     -> List(ActionButterflyDance, ActionButterflyCocoon, ActionButterflyReverseFly),
      RoomPhaseEnum.MAGIC                -> List(ActionButterflyDance, ActionButterflyCocoon, ActionButterflyReverseFly)
    )
}
