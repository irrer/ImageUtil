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

  def XdrawTextCenteredAt(graphics: Graphics2D, x: Double, y: Double, text: String) = {
    val rect = getTextDimensions(graphics, text)

    val xPos = (x - rect.getX).toFloat // adjust horizontal
    val yPos = (y + rect.getY).toFloat // adjust vertical, point to south west corner of text

    //    val xPos = (x - textSize.getWidth / 2).toFloat // adjust horizontal
    //    val yPos = (y - (textSize.getHeight / 2)).toFloat - 12 // adjust vertical, point to south west corner of text
    graphics.drawString(text, xPos, yPos) // TODO fix to xPos, yPos
  }

  def drawTextCenteredAt(graphics: Graphics2D, x: Double, y: Double, text: String) = {

    //  val metrics = graphics.getFontMetrics

    val rect = getTextDimensions(graphics, text)

    val xPos = (x - rect.getX).toFloat // adjust horizontal
    val yPos = (y + rect.getY).toFloat // adjust vertical, point to south west corner of text

    //    val xPos = (x - textSize.getWidth / 2).toFloat // adjust horizontal
    //    val yPos = (y - (textSize.getHeight / 2)).toFloat - 12 // adjust vertical, point to south west corner of text
    graphics.drawString(text, xPos, yPos) // TODO fix to xPos, yPos
  }

}