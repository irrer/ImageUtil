package edu.umro.ImageUtil

import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Graphics2D
import java.awt.RenderingHints
import java.awt.Point
import java.awt.geom.Point2D
import java.security.InvalidParameterException

object ImageUtil {

  /**
   * Get the graphics for the image, setting some preferred defaults.
   */
  def getGraphics(bufferedImage: BufferedImage): Graphics2D = {
    val graphics = bufferedImage.getGraphics.asInstanceOf[Graphics2D]
    graphics.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_GASP)
    graphics
  }

  /**
   * Get the 0 - 255 brightness of a pixel by averaging the red+green+blue levels.
   */
  def brightnessOf(color: Color): Double = (color.getRed + color.getGreen + color.getBlue) / 3.0

  /**
   * Get the 0 - 255 brightness of a pixel by averaging the red+green+blue levels.
   */
  def brightnessOf(rgb: Int): Double = brightnessOf(new Color(rgb))

  /**
   * Write the given text on the image near the given pixel.  Offset it in X and Y so that it does not obscure the
   * pixel, and use an offset that will still be inside the image by putting it roughly between the pixel and the
   * center of the image.
   */
  def annotatePixel(bufferedImage: BufferedImage, x: Double, y: Double, text: String, encircle: Boolean) = {
    val radius = 2.0
    // number of pixels between text and pixel
    val margin = 4 + radius
    val size = (radius * 2) + 2

    val imageCenter = new Point2D.Double(bufferedImage.getWidth / 2.0, bufferedImage.getHeight / 2.0)

    val graphics = getGraphics(bufferedImage)

    val ascent = graphics.getFontMetrics.getAscent
    //val descent = graphics.getFontMetrics.getDescent
    val textRect = graphics.getFontMetrics.getStringBounds(text, graphics)

    val xx = if (x < imageCenter.getX) margin else -(textRect.getWidth + margin)
    val yy = if (y < imageCenter.getY) margin + ascent else -(textRect.getHeight - graphics.getFontMetrics.getAscent + margin)

    val color = {
      val width = textRect.getWidth.toInt
      val height = textRect.getHeight.toInt
      val xMin = (x + xx).toInt
      val yMin = (y + yy).toInt
      val xMax = xMin + width
      val yMax = yMin + height
      val xRange = Math.max(xMin, 0) until Math.min(xMax, bufferedImage.getWidth)
      val yRange = Math.max(yMin, 0) until Math.min(yMax, bufferedImage.getHeight)
      val rgbList = for (x <- xRange; y <- yRange) yield bufferedImage.getRGB(x, y)
      val brightness = rgbList.map(rgb => brightnessOf(rgb)).sum / (width * height)
      if (brightness < 128) Color.white else Color.black
    }
    graphics.setColor(color)
    if (encircle) graphics.drawRect((x - radius - 1).toInt, (y - radius - 1).toInt, size.toInt, size.toInt)

    graphics.drawString(text, (x + xx).toFloat, (y + yy).toFloat)
  }

  /**
   * Given an image, magnify it by the given factor by mapping each <b>1 * 1</b> pixel to a <b>factor * factor</b> pixel.
   */
  def magnify(original: BufferedImage, factor: Int): BufferedImage = {
    if (factor < 1) throw new InvalidParameterException("magnification factor must be 1 or greater.  Caller specified: " + factor)
    val magnified = new BufferedImage(original.getWidth * factor, original.getHeight * factor, original.getType)
    for (x <- (0 until original.getWidth); y <- (0 until original.getHeight)) {
      val rgb = original.getRGB(x, y)
      for (xm <- (0 until factor); ym <- (0 until factor)) magnified.setRGB(x * factor + xm, y * factor + ym, rgb)
    }
    magnified
  }

  /**
   * Find the center of mass of a set of points, indexed starting from 0.
   */
  def centerOfMass(massList: IndexedSeq[Float]): Float = {
    val massSum = massList.sum
    if (massList.isEmpty)
      0.toFloat
    else
      { massList.indices.map(i => i * massList(i)).sum } / massSum
  }

  /**
   * Given a color, map values 0 to 255 (color depth) from black (0) to <code>color</code>.
   */
  def rgbColorMap(color: Color): IndexedSeq[Int] = {
    val red = color.getRed
    val green = color.getGreen
    val blue = color.getBlue

    def iToRGB(i: Int) = {

      def toColor(c: Int, i: Int, shift: Int) = {
        val lev = (c * (i / 256.0)).round.toInt match {
          case _ if (i < 0) => 0
          case _ if (i > 255) => 255
          case i => i
        }
        lev << shift
      }

      toColor(red, i, 16) + toColor(green, i, 8) + toColor(blue, i, 0)
    }

    (0 until 256).map(i => iToRGB(i))
  }
}