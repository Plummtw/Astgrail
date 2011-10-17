package org.plummtw.astgrail.enum

import org.plummtw.astgrail.card._

object GameEnum extends Enumeration {
  type GameEnum = Value
  
  val NONE  = Value("")
  
  //val GEM      = Value("G")
  //val CRYSTAL  = Value("C")
  
  val GEM1         = Value("G")
  val CRYSTAL1     = Value("C")
  
  val GEM2         = Value("GG")
  val GEM1CRYSTAL1 = Value("GC")
  val CRYSTAL2     = Value("CC")
  
  val CRYSTAL3     = Value("CCC")
  val GEM1CRYSTAL2 = Value("GCC")
  val GEM2CRYSTAL1 = Value("GGC")
  val GEM3         = Value("GGG")
  
  val COMBINE_MAP = scala.collection.immutable.TreeMap(
    CRYSTAL1       -> "1 水晶",
    GEM1           -> "1 寶石",
    CRYSTAL2       -> "2 水晶",
    GEM1CRYSTAL1   -> "1 寶石 1 水晶",
    GEM2           -> "2 寶石",
    
    CRYSTAL3       -> "3 水晶",
    GEM1CRYSTAL2   -> "1 寶石，2 水晶",
    GEM2CRYSTAL1   -> "2 寶石，1 水晶",
    GEM3           -> "3 寶石")
  
   def combine_cname(combine_var : GameEnum.Value) : String =
    COMBINE_MAP.get(combine_var).getOrElse("")
  
  def combine_cname(combine_var : String) : String =
    COMBINE_MAP.get(GameEnum.withName(combine_var)).get
  
  implicit def gameenum2String (en : CardTypeEnum.Value) : String = en.toString
}

object RoomRoundFlagEnum extends Enumeration {
  type RoomRoundFlagEnum = Value
  
  val NONE  = Value("")
  
  val MBOLT_REVERSE = Value("R")
  val SHOCKED       = Value("S")
  
  implicit def roomroundenum2String (en : RoomRoundFlagEnum.Value) : String = en.toString
}

object UserEntryTeamFlagEnum extends Enumeration {
  type UserEntryTeamFlagEnum = Value
  
  val NONE  = Value("")
  
  val MORALGUARD = Value("G")
  
  implicit def userentryteam2String (en : UserEntryTeamFlagEnum.Value) : String = en.toString
}

object SoulMageEnum extends Enumeration {
  type SoulMageEnum = Value
  
  val NONE  = Value("")
  
  val TOMAGE   = Value("T")
  val FROMMAGE = Value("F")
  
  implicit def soulmageenum2String (en : SoulMageEnum.Value) : String = en.toString
}