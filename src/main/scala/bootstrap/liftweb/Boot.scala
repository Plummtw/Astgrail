package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common._
import http._
import js._
import sitemap._
import Loc._
import mapper._

import org.plummtw.astgrail.model._
import org.plummtw.astgrail.view._


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  //val enableAjax = LiftRules.jsArtifacts.hide("ajax-loader").cmd &
  //                 new JE.JsRaw("$('#ajax-submit').removeAttr('disabled')").cmd

  object DBVendor extends StandardDBVendor(
      Props.get("db.class").openOr(""),
      Props.get("db.url").openOr(""),
      Props.get("db.user"),
      Props.get("db.pass")) {
    override def doNotExpandBeyond = 100
    //override def poolSize          =  10
    override def maxPoolSize       =  66
  }
  
  def boot {
    // LiftRules.jsArtifacts = net.liftweb.http.js.jquery.JQuery14Artifacts
    //LiftRules.jsArtifacts = net.liftweb.http.js.jquery.JQuery13Artifacts

    // Snippet
    LiftRules.addToPackages("org.plummtw.astgrail")

    // DB
    if (!DB.jndiJdbcConnAvailable_?){
      DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)
      //LiftRules.unloadHooks.append(() => DBVendor.closeAllConnections_!())
    }

    // Model
    val schimifier_results = Schemifier.schemify(true, Schemifier.infoF _,
      Article, ArticleThread,                                           
      Action, AdminManage, CardPool, Room, RoomRound, RoomPhase, RoomPhaseLog, Talk,
      User, UserEntry, UserEntryTeam, UserIcon, UserLogin)

    schimifier_results.foreach { schimifier_result =>
      if (schimifier_result.startsWith("CREATE TABLE usericon"))
        UserIcon.create_default_usericon
    }

    // View
    LiftRules.dispatch.append {
     case Req("captcha" :: Nil, _, _) =>
       ChineseCaptchaView.captcha
    }

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd) 
                // & new JE.JsRaw("$('#ajax-submit').attr('disabled','true')").cmd)
    
    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))
  }
  
}
  

