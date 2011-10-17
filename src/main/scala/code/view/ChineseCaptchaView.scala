package org.plummtw.astgrail.view

import net.liftweb.common.{Box, Full}
import org.plummtw.astgrail.lib.ChineseTextProducer
import java.awt.image.BufferedImage
import java.awt.font.FontRenderContext

import util.Random
import net.liftweb.http.{SessionVar, LiftResponse, InMemoryResponse}
import javax.imageio.ImageIO
import java.io.{ByteArrayOutputStream, ObjectOutputStream}
import java.awt.{Graphics, RenderingHints, Color, Font}

object ChineseCaptchaAnswer extends SessionVar[String]("")

object ChineseCaptchaView {
  val DEFAULT_WIDTH = 180
  val DEFAULT_HEIGHT = 50
  val DEFAULT_FONTS = List(//"嚙賢��佗蕭�蕭", "�嚙賣�嚙� "����嚙� "�箸嚙賜�嚙踝蕭�蕭,
                           "細明體","標楷體").map(new Font(_, Font.BOLD, 40))
  val DEFAULT_COLOR = Color.BLACK

  def render(word: String, image:BufferedImage) {
    val g = image.createGraphics()

    val hints = new RenderingHints(
      RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON)
    hints.add(new RenderingHints(RenderingHints.KEY_RENDERING,
              RenderingHints.VALUE_RENDER_QUALITY))
    g.setRenderingHints(hints)

    g.setColor(DEFAULT_COLOR)
    val frc = g.getFontRenderContext()
    var startPosX = 0
    val wc = word.toCharArray()

    val generator = new Random()
    wc.foreach { element =>
      val itchar = Array[Char](element)
      val choiceFont = generator.nextInt(DEFAULT_FONTS.size)
      val itFont = DEFAULT_FONTS(choiceFont)
      g.setFont(itFont)

      val gv = itFont.createGlyphVector(frc, itchar)
      val charWitdth = gv.getVisualBounds().getWidth()

      g.drawChars(itchar, 0, itchar.length, startPosX, 35)
      startPosX = startPosX + charWitdth.asInstanceOf[Int]
    }

    val noises = generator.nextInt(3)
    for (i <- 0 until noises)
      makeNoise(image)
    gimp(image)
  }

  def captcha () : Box[LiftResponse] = {
    val img = new BufferedImage(DEFAULT_WIDTH, DEFAULT_HEIGHT, BufferedImage.TYPE_INT_ARGB)
   	val answer = ChineseTextProducer.getText()


   	val g = img.createGraphics()
    //g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 1.0f));
    g.drawImage(img, null, null)

    ChineseCaptchaAnswer.set(answer)
    render(answer, img)

    val out = new ByteArrayOutputStream
    ImageIO.write(img, "png", ImageIO.createImageOutputStream(out))
    val outImage = out.toByteArray 

    Full(InMemoryResponse(outImage,
      ("Content-Type" -> "image/png") :: ("Cache-Control" -> "private,no-cache,no-store") :: Nil,
      Nil,
      200))
  }

  private def makeNoise(image : BufferedImage ) {
    val graphics = image.createGraphics()
    val height = image.getHeight()
    val width = image.getWidth()
    val generator = new Random()
    val y1 = generator.nextInt(height) + 1
    val y2 = generator.nextInt(height) + 1
    drawLine(graphics, y1, width, y2)
  }

  private def drawLine(g : Graphics , y1 : Int , x2 : Int, y2 : Int) {
    val X1 = 0
    val _thickness = 3

    g.setColor(DEFAULT_COLOR)
    val dX = x2 - X1
    val dY = y2 - y1

    // line length
    val lineLength = java.lang.Math.sqrt(dX * dX + dY * dY)

    val scale = _thickness / (2 * lineLength)

    var ddx = -scale * dY
    var ddy = scale * dX
    ddx += (if (ddx > 0)  0.5 else -0.5)
    ddy += (if (ddy > 0)  0.5 else -0.5)
    val dx = ddx.asInstanceOf[Int]
    val dy = ddy.asInstanceOf[Int]


    val xPoints = new Array[Int](4)
    val yPoints = new Array[Int](4)

    xPoints(0) = X1 + dx
    yPoints(0) = y1 + dy
    xPoints(1) = X1 - dx
    yPoints(1) = y1 - dy
    xPoints(2) = x2 - dx
    yPoints(2) = y2 - dy
    xPoints(3) = x2 + dx
    yPoints(3) = y2 + dy

    g.fillPolygon(xPoints, yPoints, 4)
  }

  def gimp(image : BufferedImage) {
    val _vColor = Color.WHITE
    val _hColor = Color.WHITE
    val height = image.getHeight()
    val width = image.getWidth()

    val hstripes = height / 20
    val vstripes = width / 20

    val hspace = height / (hstripes + 1)
    val vspace = width / (vstripes + 1)

    val graph = image.getGraphics()

    // Draw the horizontal stripes
    for (i <-  hspace to height-1 by hspace) {
      graph.setColor(_hColor)
      graph.drawLine(0, i, width, i)
    }

    // Draw the vertical stripes
    for (i <- vspace to width-1 by vspace) {
      graph.setColor(_vColor)
      graph.drawLine(i, 0, i, height)
    }

    // Create a pixel array of the original image.
    // we need this later to do the operations on..
    val pix = new Array[Int](height * width)
    var j = 0

    for (j1 <- 0 to width-1) {
      for (k1 <- 0 to height-1) {
        pix(j) = image.getRGB(j1, k1)
        j += 1
      }
    }
    /*

    val distance = ranInt(width / 4, width / 3)

    // put the distortion in the (dead) middle
    val wMid = image.getWidth() / 2
    val hMid = image.getHeight() / 2

    // again iterate over all pixels..
    for (x <- 0 to image.getWidth()-1) {
      for (y <- 0 to image.getHeight()-1) {

        val relX = x - wMid
        val relY = y - hMid

        val d1 = java.lang.Math.sqrt(relX * relX + relY * relY)
        if (d1 < distance) {

          val j2 = wMid +
                   (((fishEyeFormula(d1 / distance) * distance) / d1) * (x - wMid)).asInstanceOf[Int]
          val k2 = hMid +
                   (((fishEyeFormula(d1 / distance) * distance) / d1) * (y - hMid)).asInstanceOf[Int]
          image.setRGB(x, y, pix(j2 * height + k2));
        }
      }
    }
    */

    graph.dispose()
  }

  private def ranInt(i : Int, j : Int) : Int = {
    val d = scala.math.random
    return (i + ((j - i) + 1) * d).asInstanceOf[Int]
  }

  private def fishEyeFormula(s : Double) : Double = {
    // implementation of:
    // g(s) = - (3/4)s3 + (3/2)s2 + (1/4)s, with s from 0 to 1.
    if (s < 0.0) {
      return 0.0
    }
    if (s > 1.0) {
      return s
    }
    return -0.75 * s * s * s + 1.5 * s * s + 0.25 * s
  }
}