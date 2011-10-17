package org.plummtw.astgrail.snippet

import _root_.net.liftweb._
import http._
import SHtml._
import util._
import Helpers._

class TestForm extends StatefulSnippet {
  def dispatch = {
    case _ => render
  }
  
  def render =  "type=submit" #> SHtml.onSubmitUnit(() => println("TEST")) 
}
