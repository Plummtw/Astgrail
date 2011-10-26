package org.plummtw.astgrail.enum

object UserEntryRoomFlagEnum extends Enumeration {
  type UserEntryRoomFlagEnum = Value
  
  val  VOTED     = Value("V")
  val  KICKED    = Value("K")
  val  AUTOVOTED = Value("A")
  val  SKIPPED    = Value("S")
  val  SUDDENDEATH = Value("D")
  val  MUSIC       = Value("M")
}


object UserEntryRoleFlagEnum extends Enumeration {
  type UserEntryRoleFlagEnum = Value
  
  val ATTACK                 = Value("A") // 本回合的行動是攻擊
  val MAGIC                  = Value("M") // 本回合的行動是魔法
  val SPECIAL                = Value("X") // 本回合的行動是特殊
  
  val SWORDSAINT_DONE        = Value("D")
  val SWORDSAINT_COMBO       = Value("C")
  val SWORDSAINT_SWORDSHADOW = Value("S")
  
  val SAINTGIRL_HOLYHEAL     = Value("H")
  
  val SAINTLANCE_NOSKYLANCE  = Value("N")
  val SAINTLANCE_NOSMITE     = Value("O")
  
  val ELEMENTALIST_IGNITE    = Value("I")
  
  val NECROMANCER_MAGIC      = Value("E")
  
  val MAGICSWORD_COMBO       = Value("C")
  val MAGICSWORD_DARKSTUN    = Value("S")
  val MAGICSWORD_DARKSTUNNED = Value("D")
  
  val ADVENTURER_ADDON       = Value("D")
  val ADVENTURER_THEFT       = Value("T")
  
  val PRAYER_MANATIDE        = Value("P")
  val PRAYER_MAGIC           = Value("G")
  
  val SWORDEMP_ANGELSOUL     = Value("N")
  val SWORDEMP_DEVILSOUL     = Value("D")
  val SWORDEMP_NOGUARDIAN    = Value("O")
  val SWORDEMP_LIBIDINALWILL_DONE = Value("L")
  
  val MONK_POWERUP                = Value("P")
  val MONK_FIRESOUL                = Value("F")
  
  val BRAVE_ROAR                   = Value("R")
  val BRAVE_STILLWATER            = Value("S")
  
  val REDKNIGHT_CONTRACT          = Value("C")
  val REDKNIGHT_DISCIPLINE        = Value("D")
}

object UserEntryFlagEnum extends Enumeration {
  type UserEntryFlagEnum = Value
  
  val    MBOLT   = Value("M")
  val    FASTBLESSED = Value("F")
  val    ROUND_SKIP  = Value("R")
  //val  SEALED  = Value("S")
  //val  VICTORY = Value("V")
  
  //val  ADVENT  = Value("A")
  //val  CHOCOLATE = Value("C")
  //val  DIABOLIC  = Value("D")
  
  //val  GUARDIAN  = Value("G")
  //val  BARRIER    = Value("B")
  
  //val  LOVER     = Value("L")
  //val  FROG      = Value("F")
  
  //val  POISON    = Value("P")
  //val  TAUNT     = Value("T")
}

object UserEntryTeamTypeEnum extends Enumeration {
  type UserEntryTeamTypeEnum = Value
  
  val  RED  = Value("R")
  val  BLUE = Value("B")
  
  val CNAME_MAP   = Map(
    RED      -> "紅隊",
    BLUE     -> "藍隊"
  )
  
  val CSS_MAP     = Map(
    RED      -> "red",
    BLUE     -> "blue"
  )
  
  def cname(team_name : UserEntryTeamTypeEnum.Value) : String= {
    CNAME_MAP.get(team_name).getOrElse("")
  }
  
  def cname(team_name : String) : String = {
    try { cname(UserEntryTeamTypeEnum.withName(team_name)) }
    catch { case e: Exception => ""}
  }
  
  def css(team_name : UserEntryTeamTypeEnum.Value) : String= {
    CSS_MAP.get(team_name).getOrElse("")
  }
  
  def css(team_name : String) : String = {
    try { css(UserEntryTeamTypeEnum.withName(team_name)) }
    catch { case e: Exception => ""}
  }
}


