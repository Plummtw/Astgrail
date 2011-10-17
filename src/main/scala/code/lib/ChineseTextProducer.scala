package org.plummtw.astgrail.lib

import java.security.SecureRandom

object ChineseTextProducer  {
  val random = new SecureRandom()
	val DEFAULT_LENGTH = 3
  val BIG5_LENGTH = 5401

  //private final TextProducer _txtProd;	// Decorator
  //txtProd = new DefaultTextProducer(length, big5_chars);

	private val big5_chars  = new Array[Char](BIG5_LENGTH)
  private var big5_chars_index = 0

  private val tmp_bytes = new Array[Byte](2)

	private def insert_big5 (hi : Int, low : Int) {
    tmp_bytes(0) = hi.asInstanceOf[Byte]
    tmp_bytes(1) = low.asInstanceOf[Byte]
    try {
       val str = new String(tmp_bytes, "Big5")
       //System.out.println(str + " " + (int)str.charAt(0) + " " + i + " " + j);
       big5_chars(big5_chars_index) = str.charAt(0)
       big5_chars_index += 1
    } catch {
       case e : Exception =>
         e.printStackTrace()
    }
  }

  for (i <- 0xa4 to 0xc5) {
    for (j <- 0x40 to 0x7e)
      insert_big5(i, j)
    for (j <- 0xa1 to 0xfe)
      insert_big5(i, j)
  }

  for (j <- 0x40 to 0x7e) {
     insert_big5(0xc6, j)
	}

  def getText() : String = {
    (for (i <- 1 to DEFAULT_LENGTH)
       yield big5_chars(random.nextInt(BIG5_LENGTH))).mkString
  }

  //println(getText())
}
