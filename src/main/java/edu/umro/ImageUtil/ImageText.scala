package edu.umro.ImageUtil

import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Font
import java.awt.RenderingHints
import java.awt.Graphics2D
import java.awt.Font
import java.awt.font.FontRenderContext
import java.awt.Dimension
import java.awt.Point
import java.awt.geom.Rectangle2D
import java.awt.geom.Point2D

object ImageText {

  /** Font of text drawn on images */
  val DefaultFont = "SansSerif"

  /** Size of text drawn on images. */
  val DefaultTextPointSize = 30

  /** Used for determining the height of the tallest character in a font. */
  private val allPrintableChars = new String((32 to 126).toList.map(c => c.asInstanceOf[Byte]).toArray)

  def getFontHeight(graphics: Graphics2D): Int = {
    val frc = new FontRenderContext(null, true, false)
    val fontHeight = graphics.getFont.getStringBounds(allPrintableChars, frc).getHeight
    Math.ceil(fontHeight).toInt
  }

  def getFontRenderContext = new FontRenderContext(null, true, false)

  def setFont(graphics: Graphics2D, fontName: String, textPointSize: Int) = {
    val font = new Font(fontName, Font.PLAIN, textPointSize)
    graphics.setFont(font)
  }

  def getTextDimensions(graphics: Graphics2D, text: String): Rectangle2D = {
    val frc = new FontRenderContext(null, true, false)
    //val rect = graphics.getFont.getStringBounds(text, frc)
    val rect = graphics.getFontMetrics.getStringBounds(text, graphics)
    rect
  }

  def drawTextCenteredAt(graphics: Graphics2D, x: Double, y: Double, text: String) = {
    val rect = getTextDimensions(graphics, text)
    // adjust vertical, point to south west corner of text
    val xPos = (x - rect.getWidth / 2).toFloat // adjust horizontal
    val yPos = (y + (rect.getHeight - graphics.getFontMetrics.getAscent + graphics.getFontMetrics.getDescent) / 2).toFloat

    graphics.drawString(text, xPos, yPos) // TODO fix to xPos, yPos
  }

  /**
   * put a label next to a point without obscuring the point.
   *
   *  @param graphics: Draw in this context
   *
   *  @param x: Position of X coordinate
   *
   *  @param y: Position of y coordinate
   *
   *  @param degrees: Direction from which to put text.  0 degrees is to the right, 90 is
   *  above.  This value will be rounded off to the nearest 45 degrees.
   */
  def drawTextOffsetFrom(graphics: Graphics2D, x: Double, y: Double, text: String, degrees: Int) = {
    val origin = new Point2D.Double(0, 0)
    val margin = 4
    val rect = getTextDimensions(graphics, text)
    val w = rect.getWidth
    val h = rect.getHeight
    val w2 = w / 2
    val h2 = h / 2

    // round angle to nearest 45 degrees
    val deg = (3600 - degrees) % 360
    val deg45 = (0 until 360 by 45).toSeq.minBy(d => (d - deg).abs)
    val radians = deg45.toRadians

    val direction = deg45 match {
      case 0 => (1, 0)
      case 45 => (1, 1)
      case 90 => (0, 1)
      case 135 => (-1, 1)
      case 180 => (-1, 0)
      case 225 => (-1, -1)
      case 270 => (0, -1)
      case 315 => (1, -1)
      case _ => (2, 2)
    }

    val offset = new Point2D.Double(direction._1 * (margin + (w / 2)), direction._2 * (margin + (h / 2)))

    drawTextCenteredAt(graphics, x + offset.getX, y + offset.getY, text)
  }

}