package edu.umro.ImageUtil

import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Font
import java.awt.RenderingHints
import java.awt.Graphics2D
import java.awt.Font
import java.awt.font.FontRenderContext

object ImageText {

  /** Font of text drawn on images */
  val DefaultFont = "SansSerif"

  /** Size of text drawn on images. */
  val DefaultTextPointSize = 30

  def getFontHeight(font: Font): Int = {
    val allPrintableChars = new String((32 to 126).toList.map(c => c.asInstanceOf[Byte]).toArray)
    val frc = new FontRenderContext(null, true, false)
    val fontHeight = font.getStringBounds(allPrintableChars, frc).getHeight
    if (fontHeight.toInt == fontHeight) fontHeight.toInt else fontHeight.toInt + 1
  }

  def getFontRenderContext = new FontRenderContext(null, true, false)

  private def getGraphics(bufferedImage: BufferedImage): Graphics2D = bufferedImage.getGraphics.asInstanceOf[Graphics2D]

  def setFont(bufferedImage: BufferedImage, fontName: String, textPointSize: Int) = {
    val graphics = getGraphics(bufferedImage)
    graphics.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_GASP)
    val font = new Font(fontName, Font.PLAIN, textPointSize)
    graphics.setFont(font)
  }

  def annotateImage(bufferedImage: BufferedImage, x: Int, y: Int, color: Color, text: String) = {
    val graphics = getGraphics(bufferedImage)
    ???  // TODO
  }

}