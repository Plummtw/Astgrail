package org.plummtw.astgrail.util

import _root_.net.liftweb._
import net.liftweb.mapper._
import http._
import SHtml._
import js._
import JsCmds._
import common._
import util._
import Helpers._

import scala.xml.NodeSeq
// import scala.xml.Unparsed

import org.plummtw.astgrail.enum._
import org.plummtw.astgrail.model._
import org.plummtw.astgrail.util._

object UserEntryTeamHelper {
  def team_line(usrentryteam : UserEntryTeam) = {
    <tr><td class={UserEntryTeamTypeEnum.css(usrentryteam.team_type.is)}>
      { UserEntryTeamTypeEnum.cname(usrentryteam.team_type.is) + "：" +
        "　士氣：" + usrentryteam.moral.is.toString + 
        "　星杯：" + usrentryteam.grails.is.toString + 
        "　寶石：" + usrentryteam.gems.is.toString +
        "　水晶：" + usrentryteam.crystals.is.toString}
    </td></tr>
  }
  
  def team_table(room: Room, userentryteams: List[UserEntryTeam]) = {
    //val room = Room_R.get
    //val location_str = room.room_arrange.is
    val red_teams  = userentryteams.filter(_.team_type.is == UserEntryTeamTypeEnum.RED.toString)
    val blue_teams = userentryteams.filter(_.team_type.is == UserEntryTeamTypeEnum.BLUE.toString)
    
    if ((red_teams.length != 1) || (blue_teams.length != 1))
      NodeSeq.Empty
    else {
      //val userentrys = UserEntrys_RR.get
      val red_team  = red_teams(0)
      val blue_team = blue_teams(0)
    
      Seq (<table border="1"> {
        for (team <- List(red_team, blue_team)) yield
          team_line(team)
      } </table> )
    }
  }  

      
}