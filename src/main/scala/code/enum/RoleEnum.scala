package org.plummtw.astgrail.enum

import org.plummtw.astgrail.data._

/*
object RoleSideEnum extends Enumeration {
  type RoleSideEnum = Value

  val NONE     = Value("")
  val SHADOW   = Value("S")
  val HUNTER   = Value("H")
  val NEUTRAL  = Value("N")
  
  def ROLESIDE_COLOR_MAP   = scala.collection.immutable.TreeMap(
    NONE   -> "none",
    SHADOW -> "shadow",
    HUNTER -> "hunter",
    NEUTRAL -> "neutral"
  )
  
  def get_roleside_color(roleside : RoleSideEnum.Value) : String = {
    ROLESIDE_COLOR_MAP.get(roleside).getOrElse("")
  }
  
  implicit def rolesideenum2String (en : RoleSideEnum.Value) : String = en.toString
}
  弓之女神 技 Lv3
  暗殺者 技 Lv3
  劍聖 技 Lv3
  天使 聖 Lv3
  聖女 聖 Lv3
  狂戰士 血 Lv3
  封印師 幻 Lv3 專
  魔導師 詠 Lv3
  仲裁者 血 Lv3.5 擴
  魔劍 幻 Lv3.5
  死靈法師 幻 Lv3.5
  冒險家 幻 Lv3.5
  元素師 詠 Lv3.5
  聖槍 聖 Lv3.5 擴
  靈符師 詠 Lv4 擴
  神官 聖 Lv4 擴
  祈禱師 詠 Lv4
  賢者   詠 Lv4
  劍帝 技 Lv4.5 擴
  格鬥家 技 Lv4.5 擴
  勇者 血 Lv4.5 擴、專
  靈魂術士 幻 Lv4.5 擴、專
  蝶舞者 詠 Lv5 擴
  巫女 血 Lv5 擴、專
*/

object RoleEnum extends Enumeration {
  type RoleEnum = Value

  val NONE      = Value("")
  
  // Lv3
  val ARCHER    = Value("AR")
  val ASSASSIN  = Value("AS")
  val SWORDSAINT = Value("SS")
  val ANGEL      = Value("AN")
  val SAINTGIRL  = Value("SG")
  val BERSERKER  = Value("BE")
  val SEALER     = Value("SR")
  val MAGE        = Value("MA")
  
  // Lv3.5
  val JUDICATOR   = Value("JU")
  val MAGICSWORD = Value("MS")
  val NECROMANCER = Value("NE")
  val ADVENTURER  = Value("AD")
  val ELEMENTALIST = Value("EL")
  val SAINTLANCE = Value("SL")
  
  // Lv4
  val RUNEMAGE    = Value("RM")
  val BISHOP      = Value("BI")
  val PRAYER       = Value("PR")
  val SAGE         = Value("SA")
  val REDKNIGHT    = Value("RK")
  
  // Lv4.5
  val SWORDEMP   = Value("SE")
  val MONK       = Value("MO")
  val BRAVE      = Value("BR")
  val SOULMAGE   = Value("SM")
  
  // Lv5
  val MIKO      = Value("MI")
  val BUTTERFLY   = Value("BU")
  
  val ROLE_MAP   = scala.collection.immutable.TreeMap(
    NONE     -> RoleNone,
    
    ARCHER    -> RoleArcher,
    ASSASSIN  -> RoleAssassin,
    SWORDSAINT -> RoleSwordSaint,
    ANGEL      -> RoleAngel,
    SAINTGIRL  -> RoleSaintGirl,
    BERSERKER  -> RoleBerserker,
    SEALER     -> RoleSealer,
    MAGE       -> RoleMage,
  
    JUDICATOR   -> RoleJudicator,
    MAGICSWORD  -> RoleMagicSword,
    NECROMANCER -> RoleNecromancer,
    ADVENTURER   -> RoleAdventurer,
    ELEMENTALIST -> RoleElementalist,
    SAINTLANCE   -> RoleSaintLance,
  
    RUNEMAGE    -> RoleRuneMage,
    BISHOP      -> RoleBishop,
    PRAYER      -> RolePrayer,
    SAGE        -> RoleSage,
    REDKNIGHT   -> RoleRedKnight,
  
    SWORDEMP    -> RoleSwordEmp,
    MONK        -> RoleMonk,
    BRAVE       -> RoleBrave,
    SOULMAGE    -> RoleSoulMage,
  
    MIKO        -> RoleMiko,
    BUTTERFLY   -> RoleButterfly
  )
  
  val ALL_ROLE_LIST = List(
    ARCHER, ASSASSIN, SWORDSAINT, ANGEL, SAINTGIRL, BERSERKER, SEALER, MAGE,
    JUDICATOR, MAGICSWORD, NECROMANCER, ADVENTURER, ELEMENTALIST, SAINTLANCE,
    RUNEMAGE, BISHOP, PRAYER, SAGE, REDKNIGHT,
    SWORDEMP, MONK, BRAVE, SOULMAGE,
    MIKO, BUTTERFLY)
  
  lazy val WISH_ROLE_LIST = List(NONE) ::: ALL_ROLE_LIST
  
  def get_role(role : RoleEnum.Value) : RoleData = {
    val result = ROLE_MAP.get(role) 
    //if (result.isEmpty)
    //  println(role.toString + "is null")
    return result.getOrElse(RoleNone)
  }
  
  def get_role(role_string : String) : RoleData = {
    try {get_role(withName(role_string)) }
    catch {case e : Exception => RoleNone}
  }
  
  implicit def roleenum2String (en : RoleEnum.Value) : String = en.toString
}

