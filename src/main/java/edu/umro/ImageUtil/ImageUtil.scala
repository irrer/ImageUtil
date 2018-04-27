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

  def annotatePixel(bufferedImage: BufferedImage, x: Double, y: Double, color: Color, text: String) = {
    val radius = 2.0
    // number of pixels between text and pixel
    val margin = 2 + radius
    val size = (radius * 2) + 2
    val color = Color.YELLOW

    def findEmptySpace(xCenter: Double, yCenter: Double): Point = {

      val imageCenter = new Point2D.Double(bufferedImage.getWidth / 2.0, bufferedImage.getHeight / 2.0)

      case class Posn(xPos: Double, yPos: Double) {
        // lower values are better
        val rating = new Point2D.Double(xPos, yPos).distance(imageCenter)
        override def toString = "x,y: " + xPos + ", " + yPos + "    rating: " + rating.formatted("%6.1f")
      }

      val possibleLocations = Seq(
        new Posn(x, y - yCenter - margin), // north
        new Posn(x, y + yCenter + margin), // south
        new Posn(x + xCenter + margin, y), // east
        new Posn(x - xCenter - margin, y) // west
      )

      val best = possibleLocations.sortWith((a, b) => a.rating < b.rating).head

      new Point(best.xPos.ceil.toInt, best.yPos.ceil.toInt)
    }

    val graphics = getGraphics(bufferedImage)

    graphics.setColor(color)
    graphics.drawRect((x - radius - 1).toInt, (y - radius - 1).toInt, size.toInt, size.toInt)

    val textDimensions = ImageText.getTextDimensions(graphics, text)
    val textPosition = findEmptySpace(textDimensions.width / 2.0, textDimensions.height)

    ImageText.drawTextCenteredAt(graphics, textPosition.x, textPosition.y, text) // TODO put back
  }

  def magnify(original: BufferedImage, factor: Int): BufferedImage = {
    if (factor < 1) throw new InvalidParameterException("magnification factor must be 1 or greater.  Caller specified: " + factor)
    val magnified = new BufferedImage(original.getWidth * factor, original.getHeight * factor, original.getType)
    for (x <- (0 until original.getWidth); y <- (0 until original.getHeight)) {
      val rgb = original.getRGB(x, y)
      for (xm <- (0 until factor); ym <- (0 until factor)) magnified.setRGB(x * factor + xm, y * factor + ym, rgb)
    }
    magnified
  }
}