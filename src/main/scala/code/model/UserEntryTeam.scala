package org.plummtw.astgrail.model

import net.liftweb._
import net.liftweb.mapper._
import net.liftweb.util.FieldError
import net.liftweb.http.{SHtml, SessionVar, RequestVar}
import net.liftweb.common.{Empty, Box, Full}

import scala.xml.NodeSeq
import scala.util.matching.Regex

import org.plummtw.astgrail.enum._
import org.plummtw.astgrail.data._
import org.plummtw.astgrail.util.PlummUtil
import org.plummtw.astgrail.heavy.GameProcessor

//object UserEntryTeams_E        extends SessionVar[List[UserEntryTeam]](List())
//object UserEntryTeams_R        extends SessionVar[List[UserEntryTeam]](List())

class UserEntryTeam extends LongKeyedMapper[UserEntryTeam] with CreatedUpdated with IdPK {
  def getSingleton = UserEntryTeam // what's the "meta" object
  //def primaryKeyField = id

  // the primary key
  //object id            extends MappedLongIndex(this)
  object room_id       extends MappedLongForeignKey(this, Room)
  
  object team_type     extends MappedString(this, 1)
  def    team_css = {
    UserEntryTeamTypeEnum.css(team_type.is)
  }
  
  object moral         extends MappedInt(this) {
    override def defaultValue = 15
  }
  
  object grails        extends MappedInt(this) {
    override def defaultValue = 0
  }
  
  object gems         extends MappedInt(this) {
    override def defaultValue = 0
  }
  def add_gems(num : Int = 1) = {
    val num1 = math.min(num, 5 - (crystals.is + gems.is))
    if (num1 > 0)
      gems(gems.is + num1)
    this
  }

  object crystals       extends MappedInt(this) {
    override def defaultValue = 0
  }
  def add_crystals(num : Int = 1) = {
    val num1 = math.min(num, 5 - (crystals.is + gems.is))
    if (num1 > 0)
      crystals(crystals.is + num1)
    this
  }
  
  object team_flags   extends MappedString(this, 20)
  def has_team_flag(flag : UserEntryTeamFlagEnum.Value) : Boolean = 
    return (team_flags.is.indexOf(flag.toString) != -1)
  def hasnt_team_flag(flag : UserEntryTeamFlagEnum.Value) : Boolean = 
    !has_team_flag(flag)
  def add_team_flag(flag : UserEntryTeamFlagEnum.Value) : UserEntryTeam = 
    team_flags(team_flags.is + flag.toString)
  def remove_team_flag(flag : UserEntryTeamFlagEnum.Value) : UserEntryTeam = 
    team_flags(team_flags.is.replace(flag.toString, ""))
}

object UserEntryTeam extends UserEntryTeam with LongKeyedMetaMapper[UserEntryTeam] {
  override def fieldOrder = List(id, room_id, team_type,
                                 moral, grails, gems,
                                 crystals, team_flags)

  def get(team_id : Long, userentryteams : List[UserEntryTeam]) =
    userentryteams.find(_.id.is == team_id).getOrElse(GlobalUserEntryTeam.NoUserEntryTeam)
}

object GlobalUserEntryTeam{
  val NoUserEntryTeam    = UserEntryTeam.create.team_type("")
}