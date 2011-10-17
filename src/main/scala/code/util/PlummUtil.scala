package org.plummtw.astgrail.util

import java.security._
import xml.NodeSeq
import net.liftweb.http.SHtml.ChoiceItem
import net.liftweb.http.S
import net.liftweb.common.Full
//import sun.misc.BASE64Encoder
import org.apache.commons.codec.digest.DigestUtils
import org.apache.commons.codec.binary.Base64


object PlummUtil {
  val html_encode_hash = Map[Any, String] (
    '&' -> "&amp;",
    '<' -> "&lt;",
    '>' -> "&gt;",
    '"' -> "&quot;",
    '\r' -> "<br/>",
    '\n' -> "<br/>"
  )

  def  hasHtmlCode(string: String) : Boolean = {
    val count          = string.length - 1

    for (i <- 0 to count) {
      val c = string.charAt(i)
      if (html_encode_hash.isDefinedAt(c))
        return true
    }
    return false
  }

  def  encodeHtml(string: String, limit: Int) : String = {
    // .replaceAll("\'","&apos;")

    /*
    string.replaceAll("&","&amp;").replaceAll("<","&lt;").replaceAll(">","&gt;")
          .replaceAll("\"","&quot;").
           replaceAll("\r\n","\n").replaceAll("\r","\n").replaceAll("\n","<br/>")
    */
    val string_builder = new StringBuilder
    val count          = string.length - 1

    var is_escape_r    = false
    for (i <- 0 to count) {
      val c = string.charAt(i)
      val h = html_encode_hash.get(c).getOrElse(c.toString)
      if (is_escape_r && (c == '\n')) {
        is_escape_r = false
      } else {
        if (string_builder.length + h.length > limit)
          return string_builder.toString
        string_builder.append(h)
        is_escape_r = ( c == '\r')
      }
    }

    return string_builder.toString
  }

  def getIpAddress(request: net.liftweb.http.Req) : String = {

    val ip_from_header = request.headers.find{_._1 == "X-Forwarded-For"}

    val ip = ip_from_header match {
      case Some(a) => a._2
      case x       => "unknown"
    }
    /*
    {
      ip = request.getHeader( "Proxy-Client-IP" )
    }
    if ((ip == null) || (ip.length() == 0) || ("unknown".equalsIgnoreCase(ip)))   {
      ip = request.getHeader( "WL-Proxy-Client-IP" )
    }
    if ((ip == null) || (ip.length() == 0) || ("unknown".equalsIgnoreCase(ip)))   {
      ip = request.getRemoteAddr()
    }
    */

    return ( if (ip == "unknown") request.remoteAddr else ip )
  }

  def getIpAddress() : String = {
    val ipaddress = S.request match {
        case Full(x) => getIpAddress(x)
        case _ => ""
    }
    ipaddress
  }

  def htmlize (c: ChoiceItem[String], map : Map[String, String]) : NodeSeq =
    (<span>{c.xhtml} {map.get(c.key).getOrElse("")} </span>)

  def generateMD5(string: String) : String = {
    val sha = MessageDigest.getInstance("MD5");
    sha.update(string.getBytes())

    val digest = sha.digest();
    //return new BASE64Encoder().encode(digest)
    return new String(Base64.encodeBase64(digest))
  }
  
    def generateSHA1(string: String) : String = {
    val sha = MessageDigest.getInstance("SHA-1");
    sha.update(string.getBytes())

    val digest = sha.digest();
    //return new BASE64Encoder().encode(digest)
    return new String(Base64.encodeBase64(digest))
  }

  def byte2string(byte: Byte) : String = {
    val posivite_byte : Int =
      if (byte >= 0)
        byte
      else
        256 + (byte.asInstanceOf[Int])
    val high_value = posivite_byte / 16
    val low_value  = posivite_byte % 16
    return high_value.toHexString + low_value.toHexString
  }

  def bytes2string(bytes: Array[Byte]) : String = {
    val result = new StringBuffer("")
    bytes.foreach { byte => result.append(byte2string(byte)) }
    return result.toString
  }

  def generateSHA1_php(string: String) : String = {
    //val sha = MessageDigest.getInstance("SHA-1");
    //sha.update(string.getBytes())

    //val digest = sha.digest();
    //return new BASE64Encoder().encode(bytes2string(digest).getBytes)
    //generateSHA1(string)
    val sha_string = DigestUtils.shaHex(string)
    return new String(Base64.encodeBase64(sha_string.getBytes))
  }
  
  def dateAddMinute(in : java.util.Date, minute : Int) : java.util.Date = {
    var calendar = java.util.Calendar.getInstance()
    calendar.setTime(in)
    calendar.add(java.util.Calendar.MINUTE, minute)
    calendar.getTime()
  }
  
  def dateAddSecond(in : java.util.Date, second : Int) : java.util.Date = {
    var calendar = java.util.Calendar.getInstance()
    calendar.setTime(in)
    calendar.add(java.util.Calendar.SECOND, second)
    calendar.getTime()
  }
  
  def parseLong(in : String) : Long =
    try {in.toLong}
    catch {case e : Exception => 0}
    
  def parseInt(in : String) : Int =
    try {in.toInt}
    catch {case e : Exception => 0}
}