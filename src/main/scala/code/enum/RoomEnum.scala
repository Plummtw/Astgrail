package org.plummtw.astgrail.enum

object RoomFlagEnum extends Enumeration {
  val TEST_MODE      = Value("TM_")
  val WISH_ALIGN     = Value("WA_")
  val WISH_ROLE      = Value("WR_")
  //val DEATH_LOOK     = Value("DL_")
  //val EXPANSION_ROLE = Value("R1_")
  //val CUSTOM_ROLE     = Value("R2_")
  //val INIT_LOCATION     = Value("IL_")
  //val INIT_GREEN        = Value("IG_")
  val RANDOM_POSITION   = Value("RP_")
  //val FOUR_NEUTRAL      = Value("FN_")
  
  //val UNSEEN_RESIST     = Value("UE1")
  //val VAMPIRE_WEAKEN    = Value("VM1")
  //val VGHOST_EXPAND     = Value("VG1")
  //val ELLEN_HEAL        = Value("EL1")
  //val EVAN_HEAL         = Value("EV1")
  //val ANGEL_CHOOSE      = Value("AN1")
  //val BELLANDONA_CHOOSE  = Value("BE1")
  
  val FLAGNAME_MAP   = Map(
    TEST_MODE      -> "(測)",
    WISH_ALIGN     -> "(希)",
    WISH_ROLE      -> "(希職)",
    //DEATH_LOOK     -> "(靈)",
    //EXPANSION_ROLE -> "(職1)",
    //CUSTOM_ROLE    -> "(職2)",
    //INIT_LOCATION   -> "(初位)",
    //INIT_GREEN      -> "(初綠)",
    RANDOM_POSITION -> "(亂位)"
    //FOUR_NEUTRAL    -> "(四中)",
    //UNSEEN_RESIST   -> "(隱1)",
    //VAMPIRE_WEAKEN   -> "(吸1)",
    //VGHOST_EXPAND   -> "(復1)",
    //ELLEN_HEAL      -> "(蓮1)",
    //EVAN_HEAL       -> "(伊1)",
    //ANGEL_CHOOSE    -> "(天1)",
    //BELLANDONA_CHOOSE -> "(貝1)"
  )
  
  def flag_name(flag : RoomFlagEnum.Value) = {
    FLAGNAME_MAP.get(flag)
  }
  
  implicit def roomflagenum2String (en : RoomFlagEnum.Value) : String = en.toString
}

object RoomStatusEnum extends Enumeration {
  type RoomStatusEnum = Value
  
  val WAITING  = Value("W")
  val PLAYING  = Value("P")
  val ENDED    = Value("E")
  implicit def roomstatusenum2String (en : RoomStatusEnum.Value) : String = en.toString
}

object RoomVictoryEnum extends Enumeration {
  type RoomVictoryEnum = Value
  
  val NONE         = Value("")
  
  val RED_WIN      = Value("R")
  val BLUE_WIN     = Value("B")
  //val DUAL_WIN     = Value("D")
  //val NEUTRAL_WIN  = Value("N")
  //val LOVER_WIN    = Value("L")

  val DRAW         = Value("0")
  val ABANDONED    = Value("1")
  
  val VICTORY_MAP = Map(
    NONE -> "無",
    RED_WIN -> "紅隊",
    BLUE_WIN -> "藍隊",
    //DUAL_WIN   -> "雙重",
    //NEUTRAL_WIN -> "中立",
    //LOVER_WIN   -> "戀人",
    DRAW        -> "平手",
    ABANDONED   -> "廢棄"
  )
  
  def victory_name(flag : RoomVictoryEnum.Value) : String = {
    VICTORY_MAP.get(flag).getOrElse("")
  }
  
  def victory_name(flag_str : String) : String = {
    val flag = try {RoomVictoryEnum.withName(flag_str)}
      catch {case e: Exception => NONE}
    victory_name(flag)
  }
  
  implicit def roomvictoryenum2String (en : RoomVictoryEnum.Value) : String = en.toString
}

object ForceUpdateEnum extends Enumeration {
  type ForceUpdateEnum = Value
  
  val NONE            = Value("")
  
  val GO_OUT_LINK     = Value("G")
  val ACTION_BAR      = Value("A")
  val USER_TABLE      = Value("U")
  val TIME_TABLE      = Value("I")
  val TEAM_TABLE     = Value("E")
  val TALK_TABLE      = Value("T")
  val CARD_TABLE      = Value("C")

  implicit def forceupdateenum2String (en : ForceUpdateEnum.Value) : String = en.toString
}