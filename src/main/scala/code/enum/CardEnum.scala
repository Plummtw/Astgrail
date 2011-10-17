package org.plummtw.astgrail.enum

import org.plummtw.astgrail.card._

object CardTypeEnum extends Enumeration {
  type CardTypeEnum = Value
  
  val NONE  = Value("")
  //val WHITE = Value("W")
  //val BLACK = Value("B")
  //val GREEN = Value("G")
  val ATTACK = Value("A")
  val MAGIC  = Value("M")
  
  implicit def cardtypeenum2String (en : CardTypeEnum.Value) : String = en.toString
}

object CardAttrEnum extends Enumeration {
  type CardAttrEnum = Value
  
  val NONE     = Value("")
  val FIRE     = Value("F")
  val WATER    = Value("W")
  val AIR      = Value("A")
  val THUNDER  = Value("T")
  val EARTH    = Value("E")
  val LIGHT    = Value("L")
  val DARK     = Value("D")
  
  val CNAME_MAP = Map(
    NONE -> "無",
    FIRE -> "火",
    WATER -> "水",
    AIR   -> "風",
    THUNDER -> "雷",
    EARTH   -> "地",
    LIGHT   -> "光",
    DARK    -> "闇"
  )
  
  def cname(str : String) = 
    try { CNAME_MAP.get(CardAttrEnum.withName(str)).get }
    catch { case e : Exception => ""}
  
  implicit def cardattrenum2String (en : CardAttrEnum.Value) : String = en.toString
}

object CardSubAttrEnum extends Enumeration {
  type CardSubAttrEnum = Value
  
  val NONE      = Value("")
  val CAST      = Value("C")
  val BLOOD     = Value("B")
  val SKILL     = Value("S")
  val HOLY      = Value("H")
  val ILLUSION  = Value("I")
  
  implicit def cardsubattrenum2String (en : CardSubAttrEnum.Value) : String = en.toString
}

object CardMagicEnum extends Enumeration {
  type CardMagicEnum = Value
  
  val NONE      = Value("")
  val FIRE   = Value("F")
  val POISON = Value("P")
  val SHIELD = Value("S")
  val LIGHT  = Value("L")
  val WEAKEN = Value("W")
  val MBOLT  = Value("M")
  
  implicit def cardmagicsubenum2String (en : CardMagicEnum.Value) : String = en.toString
}

object CardEnum extends Enumeration {
  type CardEnum = Value
  
  val NONE                       = Value("")
  
  val FIRE_CAST                  = Value("F1")
  val FIRE_BLOOD                 = Value("F2")
  val FIRE_SKILL                 = Value("F3")
  val FIRE_HOLY                  = Value("F4")
  val FIRE_ILLUSION              = Value("F5")
  
  val WATER_CAST                 = Value("W1")
  val WATER_BLOOD                = Value("W2")
  val WATER_SKILL                = Value("W3")
  val WATER_HOLY                 = Value("W4")
  val WATER_ILLUSION1            = Value("W5")
  val WATER_ILLUSION2            = Value("W6")

  val AIR_CAST                   = Value("A1")
  val AIR_BLOOD                  = Value("A2")
  val AIR_SKILL1                 = Value("A3")
  val AIR_SKILL2                 = Value("A4")
  val AIR_HOLY                   = Value("A5")
  val AIR_ILLUSION               = Value("A6")

  val THUNDER_CAST               = Value("T1")
  val THUNDER_BLOOD              = Value("T2")
  val THUNDER_SKILL1             = Value("T3")
  val THUNDER_SKILL2             = Value("T4")
  val THUNDER_HOLY               = Value("T5")
  val THUNDER_ILLUSION           = Value("T6")

  val EARTH_CAST1                = Value("E1")
  val EARTH_CAST2                = Value("E2")
  val EARTH_BLOOD1               = Value("E3")
  val EARTH_BLOOD2               = Value("E4")
  val EARTH_SKILL                = Value("E5")
  val EARTH_HOLY1                = Value("E6")
  val EARTH_HOLY2                = Value("E7")
  val EARTH_ILLUSION1            = Value("E8")
  val EARTH_ILLUSION2            = Value("E9")
  
  val LIGHT_BLOOD                = Value("L1")
  val LIGHT_SKILL                = Value("L2")
  val LIGHT_HOLY1                = Value("L3")
  val LIGHT_HOLY2                = Value("L4")
  val LIGHT_ILLUSION             = Value("L5")
  
  val DARK_CAST                  = Value("D1")
  val DARK_HOLY                  = Value("D2")
  
  val POISON_WATER_SKILL         = Value("P1")
  val POISON_WATER_HOLY          = Value("P2")
  val POISON_WATER_ILLUSION      = Value("P3")
  val POISON_AIR_HOLY            = Value("P4")
  val POISON_THUNDER_SKILL       = Value("P5")
  val POISON_EARTH_CAST          = Value("P6")
  
  val SHIELD_FIRE_BLOOD          = Value("S1")
  val SHIELD_FIRE_SKILL          = Value("S2")
  val SHIELD_AIR_CAST            = Value("S3")
  val SHIELD_AIR_BLOOD           = Value("S4")
  val SHIELD_AIR_HOLY            = Value("S5")
  val SHIELD_THUNDER_BLOOD       = Value("S6")
  val SHIELD_THUNDER_ILLUSION    = Value("S7")
  val SHIELD_EARTH_CAST          = Value("S8")
  val SHIELD_EARTH_HOLY          = Value("S9")
  val SHIELD_EARTH_ILLUSION      = Value("S0")
  
  val WEAKEN_FIRE_BLOOD          = Value("V1")
  val WEAKEN_FIRE_HOLY           = Value("V2")
  val WEAKEN_WATER_CAST          = Value("V3")
  val WEAKEN_WATER_BLOOD         = Value("V4")
  val WEAKEN_AIR_ILLUSION        = Value("V5")
  val WEAKEN_EARTH_SKILL         = Value("V6")
  
  val MBOLT_FIRE_CAST            = Value("M1")
  val MBOLT_WATER_BLOOD          = Value("M2")
  val MBOLT_WATER_ILLUSION       = Value("M3")
  val MBOLT_AIR_ILLUSION         = Value("M4")
  val MBOLT_THUNDER_SKILL        = Value("M5")
  val MBOLT_THUNDER_HOLY         = Value("M6")
  
  lazy val CARD_LIST = CARD_COUNT_MAP.keys.map(x => 
   List.tabulate(CARD_COUNT_MAP.get(x).get)(y => x)).toList.flatten
  
  val CARD_COUNT_MAP = scala.collection.immutable.TreeMap(
    FIRE_CAST                  -> 4,
    FIRE_BLOOD                 -> 4,
    FIRE_SKILL                 -> 4,
    FIRE_HOLY                  -> 4,
    FIRE_ILLUSION              -> 5,
  
    WATER_CAST                 -> 6,
    WATER_BLOOD                -> 4,
    WATER_SKILL                -> 4,
    WATER_HOLY                 -> 3,
    WATER_ILLUSION1            -> 2,
    WATER_ILLUSION2            -> 2,

    AIR_CAST                   -> 5,
    AIR_BLOOD                  -> 4,
    AIR_SKILL1                 -> 3,
    AIR_SKILL2                 -> 2,
    AIR_HOLY                   -> 3,
    AIR_ILLUSION               -> 4,

    THUNDER_CAST               -> 4,
    THUNDER_BLOOD              -> 4,
    THUNDER_SKILL1             -> 2,
    THUNDER_SKILL2             -> 2,
    THUNDER_HOLY               -> 4,
    THUNDER_ILLUSION           -> 5,

    EARTH_CAST1                -> 2,
    EARTH_CAST2                -> 2,
    EARTH_BLOOD1               -> 2,
    EARTH_BLOOD2               -> 3,
    EARTH_SKILL                -> 5,
    EARTH_HOLY1                -> 1,
    EARTH_HOLY2                -> 2,
    EARTH_ILLUSION1            -> 2,
    EARTH_ILLUSION2            -> 2,
  
    LIGHT_BLOOD                -> 3,
    LIGHT_SKILL                -> 3,
    LIGHT_HOLY1                -> 1,
    LIGHT_HOLY2                -> 2,
    LIGHT_ILLUSION             -> 2,
  
    DARK_CAST                  -> 2,
    DARK_HOLY                  -> 4,
  
    POISON_WATER_SKILL         -> 1,
    POISON_WATER_HOLY          -> 1,
    POISON_WATER_ILLUSION      -> 1,
    POISON_AIR_HOLY            -> 1,
    POISON_THUNDER_SKILL       -> 1,
    POISON_EARTH_CAST          -> 1,
  
    SHIELD_FIRE_BLOOD          -> 1,
    SHIELD_FIRE_SKILL          -> 1,
    SHIELD_AIR_CAST            -> 1,
    SHIELD_AIR_BLOOD           -> 1,
    SHIELD_AIR_HOLY            -> 1,
    SHIELD_THUNDER_BLOOD       -> 1,
    SHIELD_THUNDER_ILLUSION    -> 1,
    SHIELD_EARTH_CAST          -> 1,
    SHIELD_EARTH_HOLY          -> 1,
    SHIELD_EARTH_ILLUSION      -> 1,
  
    WEAKEN_FIRE_BLOOD          -> 1,
    WEAKEN_FIRE_HOLY           -> 1,
    WEAKEN_WATER_CAST          -> 1,
    WEAKEN_WATER_BLOOD         -> 1,
    WEAKEN_AIR_ILLUSION        -> 1,
    WEAKEN_EARTH_SKILL         -> 1,
  
    MBOLT_FIRE_CAST            -> 1,
    MBOLT_WATER_BLOOD          -> 1,
    MBOLT_WATER_ILLUSION       -> 1,
    MBOLT_AIR_ILLUSION         -> 1,
    MBOLT_THUNDER_SKILL        -> 1,
    MBOLT_THUNDER_HOLY         -> 1
  )
  
  val CARD_MAP  = scala.collection.immutable.TreeMap(
    NONE                      -> CardNone, 
    
    FIRE_CAST                  -> CardFireCast,
    FIRE_BLOOD                 -> CardFireBlood,
    FIRE_SKILL                 -> CardFireSkill,
    FIRE_HOLY                  -> CardFireHoly,
    FIRE_ILLUSION              -> CardFireIllustion,
  
    WATER_CAST                 -> CardWaterCast,
    WATER_BLOOD                -> CardWaterBlood,
    WATER_SKILL                -> CardWaterSkill,
    WATER_HOLY                 -> CardWaterHoly,
    WATER_ILLUSION1            -> CardWaterIllustion1,
    WATER_ILLUSION2            -> CardWaterIllustion2,

    AIR_CAST                   -> CardAirCast,
    AIR_BLOOD                  -> CardAirBlood,
    AIR_SKILL1                 -> CardAirSkill1,
    AIR_SKILL2                 -> CardAirSkill12,
    AIR_HOLY                   -> CardAirHoly,
    AIR_ILLUSION               -> CardAirIllusion,

    THUNDER_CAST               -> CardThunderCast,
    THUNDER_BLOOD              -> CardThunderBlood,
    THUNDER_SKILL1             -> CardThunderSkill1,
    THUNDER_SKILL2             -> CardThunderSkill2,
    THUNDER_HOLY               -> CardThunderHoly,
    THUNDER_ILLUSION           -> CardThunderIllusion,

    EARTH_CAST1                -> CardEarthCast1,
    EARTH_CAST2                -> CardEarthCast2,
    EARTH_BLOOD1               -> CardEarthBlood1,
    EARTH_BLOOD2               -> CardEarthBlood2,
    EARTH_SKILL                -> CardEarthSkill,
    EARTH_HOLY1                -> CardEarthHoly1,
    EARTH_HOLY2                -> CardEarthHoly2,
    EARTH_ILLUSION1            -> CardEarthIllustion1,
    EARTH_ILLUSION2            -> CardEarthIllustion2,
  
    LIGHT_BLOOD                -> CardLightBlood,
    LIGHT_SKILL                -> CardLightSkill,
    LIGHT_HOLY1                -> CardLightHoly1,
    LIGHT_HOLY2                -> CardLightHoly2,
    LIGHT_ILLUSION             -> CardLightIllustion,
  
    DARK_CAST                  -> CardDarkCast,
    DARK_HOLY                  -> CardDarkHoly,
  
    POISON_WATER_SKILL         -> CardPoisonWaterSkill,
    POISON_WATER_HOLY          -> CardPoisonWaterHoly,
    POISON_WATER_ILLUSION      -> CardPoisonWaterIllusion,
    POISON_AIR_HOLY            -> CardPoisonAirHoly,
    POISON_THUNDER_SKILL       -> CardPoisonThunderSkill,
    POISON_EARTH_CAST          -> CardPoisonEarthCast,
  
    SHIELD_FIRE_BLOOD          -> CardShieldFireBlood,
    SHIELD_FIRE_SKILL          -> CardShieldFireSkill,
    SHIELD_AIR_CAST            -> CardShieldAirCast,
    SHIELD_AIR_BLOOD           -> CardShieldAirBlood,
    SHIELD_AIR_HOLY            -> CardShieldAirHoly,
    SHIELD_THUNDER_BLOOD       -> CardShieldThunderBlood,
    SHIELD_THUNDER_ILLUSION    -> CardShieldThunderIllusion,
    SHIELD_EARTH_CAST          -> CardShieldEarthCast,
    SHIELD_EARTH_HOLY          -> CardShieldEarthHoly,
    SHIELD_EARTH_ILLUSION      -> CardShieldEarthIllusion,
  
    WEAKEN_FIRE_BLOOD          -> CardWeakenFireBlood,
    WEAKEN_FIRE_HOLY           -> CardWeakenFireHoly,
    WEAKEN_WATER_CAST          -> CardWeakenWaterCast,
    WEAKEN_WATER_BLOOD         -> CardWeakenWaterBlood,
    WEAKEN_AIR_ILLUSION        -> CardWeakenAirIllusion,
    WEAKEN_EARTH_SKILL         -> CardWeakenEarthSkill,
  
    MBOLT_FIRE_CAST            -> CardMBoltFireCast,
    MBOLT_WATER_BLOOD          -> CardMBoltWaterBlood,
    MBOLT_WATER_ILLUSION       -> CardMBoltWaterIllusion,
    MBOLT_AIR_ILLUSION         -> CardMBoltAirIllusion,
    MBOLT_THUNDER_SKILL        -> CardMBoltThunderSkill,
    MBOLT_THUNDER_HOLY         -> CardMBoltThunderHoly
  )

  
  def get_card(card : CardEnum.Value) : Card =
    CARD_MAP.get(card).getOrElse(CardNone)
  
  def get_card(card : String) : Card = 
    get_card(
      try { CardEnum.withName(card) }
      catch {case e : Exception => NONE})
  
  implicit def cardenum2String (en : CardEnum.Value) : String = en.toString
}

object CardPositionEnum extends Enumeration {
  type CardPositionEnum = Value
  
  val NONE  = Value("")
  val DECK    = Value("DE")
  val DISCARD = Value("DI")
  val HAND    = Value("HA")
  val FRONT    = Value("FR")
  val BACK     = Value("BA")
  
  implicit def cardtypeenum2String (en : CardTypeEnum.Value) : String = en.toString
}