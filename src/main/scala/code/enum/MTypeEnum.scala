package org.plummtw.astgrail.enum

import org.plummtw.astgrail.data._

object MTypeEnum extends Enumeration {
  type MTypeEnum = Value
  
  val NONE               = Value("")
  
  val TALK                = Value("T")
  val TALK_DAY            = Value("TD")
  val TALK_ADMIN         = Value("TA")
  val TALK_ADMIN_PRIVATE = Value("TAP")
  val TALK_HEAVEN        = Value("TH")
  val TALK_END            = Value("TE")
  
  val MESSAGE               = Value("S")
  val MESSAGE_HIDDEN       = Value("SH")
  val MESSAGE_GENERAL      = Value("S0")
  val MESSAGE_COME         = Value("S1")
  val MESSAGE_LEAVE        = Value("S2")
  val MESSAGE_KICKED       = Value("S3")
  val MESSAGE_REVOTE0      = Value("S4")
  val MESSAGE_AUTO         = Value("S5")
  val MESSAGE_DEATHSUDDEN = Value("S6")
  val MESSAGE_REVOTE       = Value("S7")
  val MESSAGE_TIMEOUT      = Value("S8")
  val MESSAGE_TIMEOUTSKIP  = Value("S9")
  
  
  val RESULT_ATTACK        = Value("RA")
  val RESULT_REATTACK      = Value("RRA")
  val RESULT_MAGIC         = Value("RM")
  val RESULT_WEAKEN        = Value("RW")
  val RESULT_POISON        = Value("RP")
  val RESULT_SEAL          = Value("RSE1")
  val RESULT_RETALIATE     = Value("RAS3")
  //val RESULT_GREENCARD    = Value("RG")
  //val RESULT_GREENREVEAL   = Value("RV")
  //val RESULT_WHITECARD    = Value("RW")
  //val RESULT_BLACKCARD    = Value("RB")

  val OBJECTION_MALE    = Value("OM")
  val OBJECTION_FEMALE  = Value("OF")
  
  val ACTION_NO_ACTION  = Value("AN")
  val ACTION_TEST_ALERT  = Value("A0")
  val ACTION_KICK        = Value("_AK")
  val ACTION_STARTGAME  = Value("_AS")

  val ACTION_LIGHT       = Value("AL")
  val ACTION_ATTACK      = Value("AA")
  val ACTION_REATTACK    = Value("ARA")
  val ACTION_SHIELD      = Value("ASH")
  val ACTION_HEAL        = Value("AH")
  //val ACTION_NOHEAL      = Value("ANH")
  
  val ACTION_ENDUREATTACK = Value("AEA")
  val ACTION_ENDUREMAGIC  = Value("AEM")
  //val ACTION_ENDUREWEAKEN = Value("AEW")
  val ACTION_SKIPTURN     = Value("AST")
    
  val ACTION_MAGIC        = Value("AM")
  val ACTION_MAGIC_POISON = Value("AMP")
  val ACTION_MAGIC_WEAKEN = Value("AMW")
  val ACTION_MAGIC_MBOLT  = Value("AMB")
  val ACTION_MAGIC_SHIELD = Value("AMS")
  //val ACTION_MBOLT       = Value("AMB")
  
  //val ACTION_WEAKENREMOVE = Value("AWR")
  //val ACTION_WEAKENTAKE   = Value("AWT")
  
  val ACTION_PURCHASE     = Value("AP")
  val ACTION_COMBINE      = Value("AC")
  val ACTION_REFINE       = Value("AR")
  
  val ACTION_DISCARD      = Value("AD")
  val ACTION_CARD_RENEW   = Value("ACR")
  
  /*
   val ARCHER    = Value("AR")
   val ASSASSIN  = Value("AS")
   val SWORDSAINT = Value("SS")
   val ANGEL      = Value("AN")
   val SAINTGIRL  = Value("SG")
   val BERSERKER  = Value("BE")
   val SEALER     = Value("SR")
   val MAGE        = Value("MA")
  */
  val ACTION_ARCHER_AIM             = Value("AR1")
  val ACTION_ARCHER_TRAP            = Value("AR2")
  val ACTION_ARCHER_PENETRATE       = Value("AR3")
  val ACTION_ARCHER_SNIPE           = Value("AR4")
  val ACTION_ASSASSIN_WATERSHADOW   = Value("AS1")
  val ACTION_ASSASSIN_SNEAK         = Value("AS2")
  val ACTION_SWORDSAINT_FASTGALE    = Value("SS1")
  val ACTION_SWORDSAINT_STRONGGALE  = Value("SS2")
  val ACTION_SWORDSAINT_COMBO       = Value("SS3")
  val ACTION_SWORDSAINT_SWORDSHADOW = Value("SS4")
  val ACTION_ANGEL_ANGELICWALL      = Value("AN1")
  val ACTION_ANGEL_WINDPURIFY       = Value("AN2")
  val ACTION_ANGEL_ANGELBLESS       = Value("AN3")
  val ACTION_ANGEL_ANGELBLESS_GIVE  = Value("AN3_")
  val ACTION_ANGEL_ANGELBOND        = Value("AN4")
  val ACTION_ANGEL_ANGELSONG        = Value("AN5")
  val ACTION_ANGEL_GODCOVER         = Value("AN6")
  val ACTION_SAINTGIRL_HEAL         = Value("SG1")
  val ACTION_SAINTGIRL_HEALLIGHT    = Value("SG2")
  val ACTION_SAINTGIRL_COMPASSION   = Value("SG3")
  val ACTION_SAINTGIRL_HOLYHEAL     = Value("SG4")
  val ACTION_BERSERKER_BLOODROAR    = Value("BE1")
  val ACTION_BERSERKER_BLOODBLADE   = Value("BE2")
  val ACTION_BERSERKER_LACERATE     = Value("BE3")
  val ACTION_SEALER_SEAL            = Value("SR1")
  val ACTION_SEALER_FIVESEAL        = Value("SR2")
  val ACTION_SEALER_BREAKSEAL       = Value("SR3")
  val ACTION_MAGE_SHOCK             = Value("MA1")
  val ACTION_MAGE_SHOCK_DISCARD     = Value("MA1_")
  val ACTION_MAGE_STORM             = Value("MA2")
  
  /*
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
  */
  
  val ACTION_ELEMENTALIST_MAGIC       = Value("EL1")
  val ACTION_ELEMENTALIST_IGNITE      = Value("EL2")
  val ACTION_ELEMENTALIST_MOONLIGHT   = Value("EL3")
  
  val ACTION_SAINTLANCE_SHINE       = Value("SL1")
  val ACTION_SAINTLANCE_RETRIBUTION = Value("SL2")
  val ACTION_SAINTLANCE_SMITE       = Value("SL3")
  val ACTION_SAINTLANCE_SKYLANCE    = Value("SL4")
  val ACTION_SAINTLANCE_EARTHLANCE  = Value("SL5")
  val ACTION_SAINTLANCE_HOLYPRAY    = Value("SL6")
  
  val ACTION_NECROMANCER_PLAGUE     = Value("NE1")
  val ACTION_NECROMANCER_DEATHTOUCH = Value("NE2")
  val ACTION_NECROMANCER_GRAVEFALL  = Value("NE3")
  
  val ACTION_MAGICSWORD_GATHER      = Value("MS1")
  val ACTION_MAGICSWORD_COMET       = Value("MS2")
  val ACTION_MAGICSWORD_DARKSTUN    = Value("MS3")
  
  val ACTION_JUDICATOR_BREAKRITUAL  = Value("JU1")
  val ACTION_JUDICATOR_FINALJUDGE   = Value("JU2")
  val ACTION_JUDICATOR_RITUAL       = Value("JU3")
  val ACTION_JUDICATOR_BALANCE      = Value("JU4")
  
  val ACTION_ADVENTURER_DECEIVE     = Value("AD1")
  val ACTION_ADVENTURER_ADDON       = Value("AD2")
  val ACTION_ADVENTURER_THEFT       = Value("AD3")
  
  val ACTION_BISHOP_HOLYBLESS       = Value("BI1")
  val ACTION_BISHOP_HOLYWATER       = Value("BI2")
  val ACTION_BISHOP_HOLYCONTRACT    = Value("BI3")
  val ACTION_BISHOP_HOLYFIELD       = Value("BI4")
  
  val ACTION_SAGE_REFLECT           = Value("SA1")
  val ACTION_SAGE_MAGICBOOK         = Value("SA2")
  val ACTION_SAGE_HOLYBOOK          = Value("SA3")
  
  val ACTION_RUNEMAGE_THUNDERRUNE   = Value("RM1")
  val ACTION_RUNEMAGE_AIRRUNE       = Value("RM2")
  val ACTION_RUNEMAGE_AIRRUNE_DISCARD = Value("RM2_")
  val ACTION_RUNEMAGE_100GHOSTS     = Value("RM3")
  val ACTION_RUNEMAGE_USERUNE       = Value("RM4")
  
  val ACTION_PRAYER_POWERBLESS     = Value("PR1")
  val ACTION_PRAYER_POWERBLESS_USE = Value("PR1_")
  val ACTION_PRAYER_FASTBLESS      = Value("PR2")
  val ACTION_PRAYER_FASTBLESS_USE  = Value("PR2_")
  val ACTION_PRAYER_SHINEBELIEVE   = Value("PR3")
  val ACTION_PRAYER_DARKBELIEVE    = Value("PR4")
  val ACTION_PRAYER_PRAY           = Value("PR5")
  val ACTION_PRAYER_MANATIDE       = Value("PR6")
  
  val ACTION_REDKNIGHT_BLOODPRAY   = Value("RK1")
  val ACTION_REDKNIGHT_BLOODFEAST  = Value("RK2")
  val ACTION_REDKNIGHT_DISCIPLINE  = Value("RK3")
  val ACTION_REDKNIGHT_BLOODCROSS  = Value("RK4")
  
  /*
  / Lv4.5
  val SWORDEMP   = Value("SE")
  val MONK       = Value("MO")
  val BRAVE      = Value("BR")
  val SOULMAGE   = Value("SM")
  
  // Lv5
  val MIKO      = Value("MI")
  val BUTTERFLY   = Value("BU")
   */
  val ACTION_SWORDEMP_SWORDKI       = Value("SE1")
  val ACTION_SWORDEMP_ANGELSOUL     = Value("SE2")
  val ACTION_SWORDEMP_DEVILSOUL     = Value("SE3")
  val ACTION_SWORDEMP_LIBIDINALWILL = Value("SE4")
  
  val ACTION_MONK_POWERUP          = Value("MO1")
  val ACTION_MONK_KISHOT            = Value("MO2")
  val ACTION_MONK_100DRAGONS       = Value("MO3")
  val ACTION_MONK_100DRAGONS_REMOVE = Value("MO3_")
  val ACTION_MONK_FIRESOUL          = Value("MO4")
  val ACTION_MONK_MONKGOD         = Value("MO5")
  
  val ACTION_BRAVE_ROAR              = Value("BR1")
  val ACTION_BRAVE_TAUNT             = Value("BR2")
  val ACTION_BRAVE_STILLWATER        = Value("BR3")
  val ACTION_BRAVE_FORBIDDEN         = Value("BR4")
  val ACTION_BRAVE_DEATHMATCH       = Value("BR5")
  
  val ACTION_SOULMAGE_SOULGIVE      = Value("SM1")
  val ACTION_SOULMAGE_SOULBURST     = Value("SM2")
  val ACTION_SOULMAGE_SOULSUMMON    = Value("SM3")
  val ACTION_SOULMAGE_SOULCONVERTY  = Value("SM4")
  val ACTION_SOULMAGE_SOULCONVERTB  = Value("SM4_")
  val ACTION_SOULMAGE_SOULMIRROR    = Value("SM5")
  val ACTION_SOULMAGE_SOULLINK      = Value("SM6")
  val ACTION_SOULMAGE_SOULLINK_TRANSFER = Value("SM6_")
  val ACTION_SOULMAGE_SOULENHANCE     = Value("SM7")
  
  val ACTION_MIKO_BLOODCRY          = Value("MI1")
  val ACTION_MIKO_STICKWITH         = Value("MI2")
  val ACTION_MIKO_BLOODSORROW       = Value("MI3")
  val ACTION_MIKO_REVERSEBLEED      = Value("MI4")
  val ACTION_MIKO_BLOODCURSE        = Value("MI5")
  
  val ACTION_BUTTERFLY_DANCE        = Value("BU1")
  val ACTION_BUTTERFLY_POISONPOWDER = Value("BU2")
  val ACTION_BUTTERFLY_PILGRIMAGE   = Value("BU3")
  val ACTION_BUTTERFLY_MIRRORFLOWER = Value("BU4")
  val ACTION_BUTTERFLY_BACKDISCARD  = Value("BU5")
  val ACTION_BUTTERFLY_COCOON       = Value("BU6")
  val ACTION_BUTTERFLY_REVERSEFLY   = Value("BU7")

  
  def get_action(string : String) : MTypeEnum.Value = {
    try {MTypeEnum.withName(string)}
    catch {case e: Exception => NONE}
  }
  
  implicit def mtypeenum2String (en : MTypeEnum.Value) : String = en.toString
}