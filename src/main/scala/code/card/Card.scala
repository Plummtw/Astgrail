package org.plummtw.astgrail.card

import org.plummtw.astgrail.enum._

trait CardMagic { def cardmagic_enum : CardMagicEnum.Value = CardMagicEnum.NONE }

class Card(val enum : CardEnum.Value, 
           val name : String, 
           val type_enum : CardTypeEnum.Value,
           val attr_enum : CardAttrEnum.Value,
           val subattr_enum : CardSubAttrEnum.Value) extends CardMagic {
  def card_enum = enum
  def card_name = name  
  def cardtype_enum = type_enum
  def cardattr_enum = attr_enum
  def cardsubattr_enum = subattr_enum
  
  val description = ""
  def card_description = description
  
  val specific_actions : List[MTypeEnum.Value] = List()
  def has_action(action : MTypeEnum.Value) = specific_actions.contains(action)
}

trait CardPoison extends CardMagic { override def cardmagic_enum = CardMagicEnum.POISON }
trait CardWeaken extends CardMagic { override def cardmagic_enum = CardMagicEnum.WEAKEN }
trait CardLight extends CardMagic  { override def cardmagic_enum = CardMagicEnum.LIGHT }
trait CardShield extends CardMagic { override def cardmagic_enum = CardMagicEnum.SHIELD }
trait CardMBolt extends CardMagic { override def cardmagic_enum = CardMagicEnum.MBOLT }


object CardNone extends Card(CardEnum.NONE, "無", CardTypeEnum.NONE, 
                             CardAttrEnum.NONE, CardSubAttrEnum.NONE)

object CardFireCast extends Card(CardEnum.FIRE_CAST, "火焰斬-詠", 
                                   CardTypeEnum.ATTACK, CardAttrEnum.FIRE, CardSubAttrEnum.CAST) {
  override val specific_actions = List(MTypeEnum.ACTION_PRAYER_POWERBLESS, MTypeEnum.ACTION_ELEMENTALIST_MAGIC)
  override val description = "(祈禱師：威力賜福，元素師：火球)"
}
object CardFireBlood extends Card(CardEnum.FIRE_BLOOD, "火焰斬-血", 
                                   CardTypeEnum.ATTACK, CardAttrEnum.FIRE, CardSubAttrEnum.BLOOD) {
  override val specific_actions = List(MTypeEnum.ACTION_BERSERKER_BLOODROAR, MTypeEnum.ACTION_MIKO_BLOODCRY)
  override val description = "(狂戰士：血腥咆哮，巫女：血之悲鳴)"
}
object CardFireSkill extends Card(CardEnum.FIRE_SKILL, "火焰斬-技", 
                                    CardTypeEnum.ATTACK, CardAttrEnum.FIRE, CardSubAttrEnum.SKILL) {
  override val specific_actions = List(MTypeEnum.ACTION_SWORDSAINT_FASTGALE, MTypeEnum.ACTION_ARCHER_TRAP)
  override val description = "(劍聖：疾風技，弓之女神：閃光陷阱)"
}
object CardFireHoly extends Card(CardEnum.FIRE_HOLY, "火焰斬-聖", 
                                   CardTypeEnum.ATTACK, CardAttrEnum.FIRE, CardSubAttrEnum.HOLY) {
  override val specific_actions = List(MTypeEnum.ACTION_SAINTGIRL_HEAL, MTypeEnum.ACTION_ANGEL_ANGELICWALL)
  override val description = "(聖女：治療術，天使：天使之牆)"
}
object CardFireIllustion extends Card(CardEnum.FIRE_ILLUSION, "火焰斬-幻", 
                                   CardTypeEnum.ATTACK, CardAttrEnum.FIRE, CardSubAttrEnum.ILLUSION) {
  override val specific_actions = List(MTypeEnum.ACTION_SEALER_SEAL, MTypeEnum.ACTION_SOULMAGE_SOULBURST)
  override val description = "(封印師：火之封印，靈魂術士：靈魂震爆)"
}

object CardWaterCast extends Card(CardEnum.WATER_CAST, "水漣斬-詠", 
                                   CardTypeEnum.ATTACK, CardAttrEnum.WATER, CardSubAttrEnum.CAST) {
  override val specific_actions = List(MTypeEnum.ACTION_PRAYER_POWERBLESS, MTypeEnum.ACTION_ELEMENTALIST_MAGIC)
  override val description = "(祈禱師：威力賜福，元素師：冰凍)"
}
object CardWaterBlood extends Card(CardEnum.WATER_BLOOD, "水漣斬-血", 
                                   CardTypeEnum.ATTACK, CardAttrEnum.WATER, CardSubAttrEnum.BLOOD) {
  override val specific_actions = List(MTypeEnum.ACTION_BERSERKER_BLOODBLADE, MTypeEnum.ACTION_MIKO_BLOODCRY)
  override val description = "(狂戰士：血影狂刀，巫女：血之悲鳴)"
}
object CardWaterSkill extends Card(CardEnum.WATER_SKILL, "水漣斬-技", 
                                    CardTypeEnum.ATTACK, CardAttrEnum.WATER, CardSubAttrEnum.SKILL) {
  override val specific_actions = List(MTypeEnum.ACTION_SWORDSAINT_FASTGALE, MTypeEnum.ACTION_ARCHER_TRAP)
  override val description = "(劍聖：疾風技，弓之女神：閃光陷阱)"
}
object CardWaterHoly extends Card(CardEnum.WATER_HOLY, "水漣斬-聖", 
                                   CardTypeEnum.ATTACK, CardAttrEnum.WATER, CardSubAttrEnum.HOLY) {
  override val specific_actions = List(MTypeEnum.ACTION_SAINTGIRL_HEALLIGHT, MTypeEnum.ACTION_ANGEL_ANGELICWALL)
  override val description = "(聖女：治癒之光，天使：天使之牆)"
}
object CardWaterIllustion1 extends Card(CardEnum.WATER_ILLUSION1, "水漣斬-幻(賜)", 
                                     CardTypeEnum.ATTACK, CardAttrEnum.WATER, CardSubAttrEnum.ILLUSION) {
  override val specific_actions = List(MTypeEnum.ACTION_SEALER_SEAL, MTypeEnum.ACTION_SOULMAGE_SOULGIVE)
  override val description = "(封印師：水之封印，靈魂術士：靈魂賜予)"
}
object CardWaterIllustion2 extends Card(CardEnum.WATER_ILLUSION2, "水漣斬-幻(爆)", 
                                     CardTypeEnum.ATTACK, CardAttrEnum.WATER, CardSubAttrEnum.ILLUSION) {
  override val specific_actions = List(MTypeEnum.ACTION_SEALER_SEAL, MTypeEnum.ACTION_SOULMAGE_SOULBURST)
  override val description = "(封印師：水之封印，靈魂術士：靈魂震爆)"
}

object CardAirCast extends Card(CardEnum.AIR_CAST, "風神斬-詠", 
                                   CardTypeEnum.ATTACK, CardAttrEnum.AIR, CardSubAttrEnum.CAST) {
  override val specific_actions = List(MTypeEnum.ACTION_PRAYER_FASTBLESS, MTypeEnum.ACTION_ELEMENTALIST_MAGIC)
  override val description = "(祈禱師：迅捷賜福，元素師：風刃)"
}
object CardAirBlood extends Card(CardEnum.AIR_BLOOD, "風神斬-血", 
                                   CardTypeEnum.ATTACK, CardAttrEnum.AIR, CardSubAttrEnum.BLOOD) {
  override val specific_actions = List(MTypeEnum.ACTION_BERSERKER_BLOODROAR, MTypeEnum.ACTION_MIKO_BLOODCRY)
  override val description = "(狂戰士：血腥咆哮，巫女：血之悲鳴)"
}
object CardAirSkill1 extends Card(CardEnum.AIR_SKILL1, "風神斬-技(疾)", 
                                    CardTypeEnum.ATTACK, CardAttrEnum.AIR, CardSubAttrEnum.SKILL) {
  override val specific_actions = List(MTypeEnum.ACTION_SWORDSAINT_FASTGALE, MTypeEnum.ACTION_ARCHER_AIM)
  override val description = "(劍聖：疾風技，弓之女神：精準射擊)"
}
object CardAirSkill12 extends Card(CardEnum.AIR_SKILL2, "風神斬-技(烈)", 
                                    CardTypeEnum.ATTACK, CardAttrEnum.AIR, CardSubAttrEnum.SKILL) {
  override val specific_actions = List(MTypeEnum.ACTION_SWORDSAINT_STRONGGALE, MTypeEnum.ACTION_ARCHER_AIM)
  override val description = "(劍聖：烈風技，弓之女神：精準射擊)"
}
object CardAirHoly extends Card(CardEnum.AIR_HOLY, "風神斬-聖", 
                                   CardTypeEnum.ATTACK, CardAttrEnum.AIR, CardSubAttrEnum.HOLY) {
  override val specific_actions = List(MTypeEnum.ACTION_SAINTGIRL_HEALLIGHT, MTypeEnum.ACTION_ANGEL_ANGELICWALL)
  override val description = "(聖女：治癒之光，天使：天使之牆)"
}
object CardAirIllusion extends Card(CardEnum.AIR_ILLUSION, "風神斬-幻", 
                                     CardTypeEnum.ATTACK, CardAttrEnum.AIR, CardSubAttrEnum.ILLUSION) {
  override val specific_actions = List(MTypeEnum.ACTION_SEALER_SEAL, MTypeEnum.ACTION_SOULMAGE_SOULGIVE)
  override val description = "(封印師：風之封印，靈魂術士：靈魂賜予)"
}

object CardThunderCast extends Card(CardEnum.THUNDER_CAST, "雷光斬-詠", 
                                   CardTypeEnum.ATTACK, CardAttrEnum.THUNDER, CardSubAttrEnum.CAST) {
  override val specific_actions = List(MTypeEnum.ACTION_PRAYER_FASTBLESS, MTypeEnum.ACTION_ELEMENTALIST_MAGIC)
  override val description = "(祈禱師：迅捷賜福，元素師：雷擊)"
}
object CardThunderBlood extends Card(CardEnum.THUNDER_BLOOD, "雷光斬-血", 
                                   CardTypeEnum.ATTACK, CardAttrEnum.THUNDER, CardSubAttrEnum.BLOOD) {
  override val specific_actions = List(MTypeEnum.ACTION_BERSERKER_BLOODBLADE, MTypeEnum.ACTION_MIKO_BLOODCRY)
  override val description = "(狂戰士：血影狂刀，巫女：血之悲鳴)"
}
object CardThunderSkill1 extends Card(CardEnum.THUNDER_SKILL1, "雷光斬-技(疾)", 
                                    CardTypeEnum.ATTACK, CardAttrEnum.THUNDER, CardSubAttrEnum.SKILL) {
  override val specific_actions = List(MTypeEnum.ACTION_SWORDSAINT_FASTGALE, MTypeEnum.ACTION_ARCHER_AIM)
  override val description = "(劍聖：疾風技，弓之女神：精準射擊)"
}
object CardThunderSkill2 extends Card(CardEnum.THUNDER_SKILL2, "雷光斬-技(烈)", 
                                    CardTypeEnum.ATTACK, CardAttrEnum.THUNDER, CardSubAttrEnum.SKILL) {
  override val specific_actions = List(MTypeEnum.ACTION_SWORDSAINT_STRONGGALE, MTypeEnum.ACTION_ARCHER_AIM)
  override val description = "(劍聖：烈風技，弓之女神：精準射擊)"
}
object CardThunderHoly extends Card(CardEnum.THUNDER_HOLY, "雷光斬-聖", 
                                   CardTypeEnum.ATTACK, CardAttrEnum.THUNDER, CardSubAttrEnum.HOLY) {
  override val specific_actions = List(MTypeEnum.ACTION_SAINTGIRL_HEAL, MTypeEnum.ACTION_ANGEL_ANGELICWALL)
  override val description = "(聖女：治療術，天使：天使之牆)"
}
object CardThunderIllusion extends Card(CardEnum.THUNDER_ILLUSION, "雷光斬-幻", 
                                     CardTypeEnum.ATTACK, CardAttrEnum.THUNDER, CardSubAttrEnum.ILLUSION) {
  override val specific_actions = List(MTypeEnum.ACTION_SEALER_SEAL, MTypeEnum.ACTION_SOULMAGE_SOULBURST)
  override val description = "(封印師：雷之封印，靈魂術士：靈魂震爆)"
}


object CardEarthCast1 extends Card(CardEnum.EARTH_CAST1, "地裂斬-詠(威)", 
                                   CardTypeEnum.ATTACK, CardAttrEnum.EARTH, CardSubAttrEnum.CAST) {
  override val specific_actions = List(MTypeEnum.ACTION_PRAYER_POWERBLESS, MTypeEnum.ACTION_ELEMENTALIST_MAGIC)
  override val description = "(祈禱師：威力賜福，元素師：隕石)"
}
object CardEarthCast2 extends Card(CardEnum.EARTH_CAST2, "地裂斬-詠(迅)", 
                                   CardTypeEnum.ATTACK, CardAttrEnum.EARTH, CardSubAttrEnum.CAST) {
  override val specific_actions = List(MTypeEnum.ACTION_PRAYER_FASTBLESS, MTypeEnum.ACTION_ELEMENTALIST_MAGIC)
  override val description = "(祈禱師：迅捷賜福，元素師：隕石)"
}
object CardEarthBlood1 extends Card(CardEnum.EARTH_BLOOD1, "地裂斬-血(咆)", 
                                   CardTypeEnum.ATTACK, CardAttrEnum.EARTH, CardSubAttrEnum.BLOOD) {
  override val specific_actions = List(MTypeEnum.ACTION_BERSERKER_BLOODROAR, MTypeEnum.ACTION_MIKO_BLOODCRY)
  override val description = "(狂戰士：血腥咆哮，巫女：血之悲鳴)"
}
object CardEarthBlood2 extends Card(CardEnum.EARTH_BLOOD2, "地裂斬-血(狂)", 
                                   CardTypeEnum.ATTACK, CardAttrEnum.EARTH, CardSubAttrEnum.BLOOD) {
  override val specific_actions = List(MTypeEnum.ACTION_BERSERKER_BLOODBLADE, MTypeEnum.ACTION_MIKO_BLOODCRY)
  override val description = "(狂戰士：血影狂刀，巫女：血之悲鳴)"
}
object CardEarthSkill  extends Card(CardEnum.EARTH_SKILL, "地裂斬-技", 
                                    CardTypeEnum.ATTACK, CardAttrEnum.EARTH, CardSubAttrEnum.SKILL)  {
  override val specific_actions = List(MTypeEnum.ACTION_SWORDSAINT_STRONGGALE, MTypeEnum.ACTION_ARCHER_AIM)
  override val description = "(劍聖：烈風技，弓之女神：精準射擊)"
}
object CardEarthHoly1 extends Card(CardEnum.EARTH_HOLY1, "地裂斬-聖(牆)", 
                                   CardTypeEnum.ATTACK, CardAttrEnum.EARTH, CardSubAttrEnum.HOLY) {
  override val specific_actions = List(MTypeEnum.ACTION_SAINTGIRL_HEALLIGHT, MTypeEnum.ACTION_ANGEL_ANGELICWALL)
  override val description = "(聖女：治癒之光，天使：天使之牆)"
}
object CardEarthHoly2 extends Card(CardEnum.EARTH_HOLY2, "地裂斬-聖(無)", 
                                   CardTypeEnum.ATTACK, CardAttrEnum.EARTH, CardSubAttrEnum.HOLY) {
  override val specific_actions = List(MTypeEnum.ACTION_SAINTGIRL_HEALLIGHT)
  override val description = "(聖女：治癒之光)"
}
object CardEarthIllustion1 extends Card(CardEnum.EARTH_ILLUSION1, "地裂斬-幻(賜)", 
                                     CardTypeEnum.ATTACK, CardAttrEnum.EARTH, CardSubAttrEnum.ILLUSION) {
  override val specific_actions = List(MTypeEnum.ACTION_SEALER_SEAL, MTypeEnum.ACTION_SOULMAGE_SOULGIVE)
  override val description = "(封印師：地之封印，靈魂術士：靈魂賜予)"
}
object CardEarthIllustion2 extends Card(CardEnum.EARTH_ILLUSION2, "地裂斬-幻(爆)", 
                                     CardTypeEnum.ATTACK, CardAttrEnum.EARTH, CardSubAttrEnum.ILLUSION) {
  override val specific_actions = List(MTypeEnum.ACTION_SEALER_SEAL, MTypeEnum.ACTION_SOULMAGE_SOULBURST)
  override val description = "(封印師：地之封印，靈魂術士：靈魂震爆)"
}

object CardLightBlood extends Card(CardEnum.LIGHT_BLOOD, "聖光-血", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.LIGHT, CardSubAttrEnum.BLOOD) with CardLight
object CardLightSkill extends Card(CardEnum.LIGHT_SKILL, "聖光-技", 
                                    CardTypeEnum.MAGIC, CardAttrEnum.LIGHT, CardSubAttrEnum.SKILL) with CardLight
object CardLightHoly1 extends Card(CardEnum.LIGHT_HOLY1, "聖光-聖(光)", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.LIGHT, CardSubAttrEnum.HOLY) with CardLight {
  override val specific_actions = List(MTypeEnum.ACTION_SAINTGIRL_HEALLIGHT)
  override val description = "(聖女：治癒之光)"
}
object CardLightHoly2 extends Card(CardEnum.LIGHT_HOLY2, "聖光-聖(術)", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.LIGHT, CardSubAttrEnum.HOLY) with CardLight {
  override val specific_actions = List(MTypeEnum.ACTION_SAINTGIRL_HEAL)
  override val description = "(聖女：治療術)"
}
object CardLightIllustion extends Card(CardEnum.LIGHT_ILLUSION, "聖光-幻", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.LIGHT, CardSubAttrEnum.HOLY) with CardLight


object CardDarkCast extends Card(CardEnum.DARK_CAST, "暗滅-詠", 
                                   CardTypeEnum.ATTACK, CardAttrEnum.DARK, CardSubAttrEnum.CAST)
object CardDarkHoly extends Card(CardEnum.DARK_HOLY, "暗滅-聖", 
                                   CardTypeEnum.ATTACK, CardAttrEnum.DARK, CardSubAttrEnum.HOLY)

object CardPoisonWaterSkill   extends Card(CardEnum.POISON_WATER_SKILL, "中毒-水技", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.WATER, CardSubAttrEnum.CAST)
                                 with CardPoison
object CardPoisonWaterHoly     extends Card(CardEnum.POISON_WATER_HOLY, "中毒-水聖", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.WATER, CardSubAttrEnum.HOLY)
                                 with CardPoison
object CardPoisonWaterIllusion extends Card(CardEnum.POISON_WATER_ILLUSION, "中毒-水幻", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.WATER, CardSubAttrEnum.ILLUSION)
                                 with CardPoison
object CardPoisonAirHoly        extends Card(CardEnum.POISON_AIR_HOLY, "中毒-風聖", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.AIR, CardSubAttrEnum.HOLY)
                                 with CardPoison
object CardPoisonThunderSkill extends Card(CardEnum.POISON_THUNDER_SKILL, "中毒-雷技", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.THUNDER, CardSubAttrEnum.SKILL)
                                 with CardPoison
object CardPoisonEarthCast    extends Card(CardEnum.POISON_EARTH_CAST, "中毒-地誦", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.EARTH, CardSubAttrEnum.CAST)
                                 with CardPoison

object CardShieldFireBlood   extends Card(CardEnum.SHIELD_FIRE_BLOOD, "聖盾-火血", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.FIRE, CardSubAttrEnum.BLOOD)
                                 with CardShield
object CardShieldFireSkill     extends Card(CardEnum.SHIELD_FIRE_SKILL, "聖盾-火技", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.FIRE, CardSubAttrEnum.SKILL)
                                 with CardShield
object CardShieldAirCast       extends Card(CardEnum.SHIELD_AIR_CAST, "聖盾-風誦", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.AIR, CardSubAttrEnum.CAST)
                                 with CardShield
object CardShieldAirBlood        extends Card(CardEnum.SHIELD_AIR_BLOOD, "聖盾-風血", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.AIR, CardSubAttrEnum.BLOOD)
                                 with CardShield
object CardShieldAirHoly         extends Card(CardEnum.SHIELD_AIR_HOLY, "聖盾-風聖", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.AIR, CardSubAttrEnum.HOLY)
                                 with CardShield
object CardShieldThunderBlood   extends Card(CardEnum.SHIELD_THUNDER_BLOOD, "聖盾-雷血", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.THUNDER, CardSubAttrEnum.BLOOD)
                                 with CardShield
object CardShieldThunderIllusion extends Card(CardEnum.SHIELD_THUNDER_ILLUSION, "聖盾-雷幻", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.THUNDER, CardSubAttrEnum.ILLUSION)
                                 with CardShield
object CardShieldEarthCast       extends Card(CardEnum.SHIELD_EARTH_CAST, "聖盾-地誦", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.EARTH, CardSubAttrEnum.CAST)
                                 with CardShield
object CardShieldEarthHoly       extends Card(CardEnum.SHIELD_EARTH_HOLY, "聖盾-地聖", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.EARTH, CardSubAttrEnum.HOLY)
                                 with CardShield
object CardShieldEarthIllusion  extends Card(CardEnum.SHIELD_EARTH_ILLUSION, "聖盾-地幻", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.EARTH, CardSubAttrEnum.ILLUSION)
                                 with CardShield
  

object CardWeakenFireBlood   extends Card(CardEnum.WEAKEN_FIRE_BLOOD, "虛弱-火血", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.FIRE, CardSubAttrEnum.BLOOD)
                                 with CardWeaken
object CardWeakenFireHoly     extends Card(CardEnum.WEAKEN_FIRE_HOLY, "虛弱-火聖", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.FIRE, CardSubAttrEnum.HOLY)
                                 with CardWeaken
object CardWeakenWaterCast     extends Card(CardEnum.WEAKEN_WATER_CAST, "虛弱-水誦", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.WATER, CardSubAttrEnum.CAST)
                                 with CardWeaken
object CardWeakenWaterBlood     extends Card(CardEnum.WEAKEN_WATER_BLOOD, "虛弱-水血", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.WATER, CardSubAttrEnum.BLOOD)
                                 with CardWeaken
object CardWeakenAirIllusion    extends Card(CardEnum.WEAKEN_AIR_ILLUSION, "虛弱-風幻", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.AIR, CardSubAttrEnum.ILLUSION)
                                 with CardWeaken
object CardWeakenEarthSkill     extends Card(CardEnum.WEAKEN_EARTH_SKILL, "虛弱-地技", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.EARTH, CardSubAttrEnum.SKILL)
                                 with CardWeaken


object CardMBoltFireCast   extends Card(CardEnum.MBOLT_FIRE_CAST, "魔彈-火誦", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.FIRE, CardSubAttrEnum.CAST)
                                 with CardMBolt
object CardMBoltWaterBlood     extends Card(CardEnum.MBOLT_WATER_BLOOD, "魔彈-水血", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.WATER, CardSubAttrEnum.BLOOD)
                                 with CardMBolt
object CardMBoltWaterIllusion     extends Card(CardEnum.MBOLT_WATER_ILLUSION, "魔彈-水幻", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.WATER, CardSubAttrEnum.ILLUSION)
                                 with CardMBolt
object CardMBoltAirIllusion     extends Card(CardEnum.MBOLT_AIR_ILLUSION, "魔彈-風幻", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.AIR, CardSubAttrEnum.ILLUSION)
                                 with CardMBolt
object CardMBoltThunderSkill    extends Card(CardEnum.MBOLT_THUNDER_SKILL, "魔彈-雷技", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.THUNDER, CardSubAttrEnum.SKILL)
                                 with CardMBolt
object CardMBoltThunderHoly     extends Card(CardEnum.MBOLT_THUNDER_HOLY, "魔彈-雷聖", 
                                   CardTypeEnum.MAGIC, CardAttrEnum.THUNDER, CardSubAttrEnum.HOLY)
                                 with CardMBolt  
