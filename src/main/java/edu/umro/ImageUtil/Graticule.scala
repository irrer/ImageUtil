package edu.umro.ImageUtil

import java.awt.BasicStroke
import java.awt.Color
import java.awt.image.BufferedImage

/**
  * Add graticules to an image.
  */
//noinspection ScalaUnusedSymbol
object Graticule {

  /**
    * Add graticules to the given image.
    */
  def addGraticules(image: BufferedImage, x2Pix: Double => Double, y2Pix: Double => Double, pix2X: Double => Double, pix2Y: Double => Double, graticuleColor: Color): Unit = {

    val graphics = ImageUtil.getGraphics(image)

    val lightlyDashedLine = new BasicStroke(1, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 0, Array(0.5f, 4), 0)

    val xMin = 0
    val yMin = 0
    val xMax = image.getWidth - 1
    val yMax = image.getHeight - 1
    val xMiddle = xMax / 2
    val yMiddle = yMax / 2

    //    val min = translator.pix2Iso(xMin, yMin)
    //    val max = translator.pix2Iso(xMax, yMax)

    val xGrt = ImageUtil.graticule(pix2X(xMin), pix2X(xMax), 8)
    val yGrt = ImageUtil.graticule(pix2Y(yMin), pix2Y(yMax), 8)

    val grtMajorLength = 10
    val grtMinorLength = 5

    graphics.setColor(graticuleColor)
    ImageUtil.setSolidLine(graphics)

    // draw borders and center lines
    graphics.drawLine(xMin, yMin, xMin, yMax)
    graphics.drawLine(xMax, yMin, xMax, yMax)
    graphics.drawLine(xMin, yMin, xMax, yMin)
    graphics.drawLine(xMin, yMax, xMax, yMax)
    graphics.drawLine(xMin, yMiddle, xMax, yMiddle)
    graphics.drawLine(xMiddle, yMin, xMiddle, yMax)

    //    def x2Pix(xIso: Double) = translator.iso2Pix(xIso, 0).getX.round.toInt
    //    def y2Pix(yIso: Double) = translator.iso2Pix(0, yIso).getY.round.toInt

    val offset = 1 // Offset of tic mark from number in pixels.

    def closeTogether(aPix: Double, bPix: Double) = (aPix - bPix).abs < 2

    val numMinorTics = 4 // number of minor tics between major tics
    val xMinorInc = (x2Pix(xGrt(1)) - x2Pix(xGrt.head)) / (numMinorTics + 1)
    val yMinorInc = (y2Pix(xGrt(1)) - y2Pix(yGrt.head)) / (numMinorTics + 1)

    def drawLine(x1: Double, y1: Double, x2: Double, y2: Double): Unit = graphics.drawLine(x1.round.toInt, y1.round.toInt, x2.round.toInt, y2.round.toInt)

    // draw top, bottom, and center graticules
    for (xIso <- xGrt) {
      val x = x2Pix(xIso)
      val minorStart = if (xIso == xGrt.head) -numMinorTics else 1
      val text = xIso.round.toInt.toString + " "

      if (!closeTogether(x, xMiddle)) { // do not overwrite middle line
        // draw light grid lines
        graphics.setStroke(lightlyDashedLine)
        drawLine(x, yMin, x, yMax)
        ImageUtil.setSolidLine(graphics)
      }

      // top
      drawLine(x, yMin, x, yMin + grtMajorLength) // draw major graticule
      for (mt <- minorStart to numMinorTics) drawLine(x + (mt * xMinorInc), yMin, x + (mt * xMinorInc), yMin + grtMinorLength)
      ImageText.drawTextOffsetFrom(graphics, x, yMin + grtMajorLength + offset, text, 270) // draw number corresponding to major graticule
      // bottom
      drawLine(x, yMax, x, yMax - grtMajorLength) // draw major graticule
      for (mt <- minorStart to numMinorTics) drawLine(x + (mt * xMinorInc), yMax, x + (mt * xMinorInc), yMax - grtMinorLength)
      ImageText.drawTextOffsetFrom(graphics, x, yMax - grtMajorLength - offset, text, 90) // draw number corresponding to major graticule
      // center horizontal
      drawLine(x, yMiddle - grtMajorLength, x, yMiddle + grtMajorLength) // draw major graticule
      for (mt <- minorStart to numMinorTics) drawLine(x + (mt * xMinorInc), yMiddle - grtMinorLength, x + (mt * xMinorInc), yMiddle + grtMinorLength)
      ImageText.drawTextOffsetFrom(graphics, x, yMiddle - grtMajorLength - offset, text, 90) // draw number corresponding to major graticule
    }

    // draw left and right graticules
    for (yIso <- yGrt) {
      val y = y2Pix(yIso).round.toInt
      val minorStart = if (yIso == yGrt.head) -numMinorTics else 1
      val text = yIso.round.toInt.toString + " "

      if (!closeTogether(y, yMiddle)) { // do not overwrite middle line
        // draw light grid lines
        graphics.setStroke(lightlyDashedLine)
        drawLine(xMin, y, xMax, y)
        ImageUtil.setSolidLine(graphics)
      }

      val textWidth = ImageText.getTextDimensions(graphics, text).getWidth.round.toInt
      // left
      drawLine(xMin, y, xMin + grtMajorLength, y) // draw major graticule
      for (mt <- minorStart to numMinorTics) drawLine(xMin, y + (mt * yMinorInc), xMin + grtMinorLength, y + (mt * yMinorInc))
      ImageText.drawTextOffsetFrom(graphics, xMin + grtMajorLength + offset, y, text, 0) // draw number corresponding to major graticule
      // right
      drawLine(xMax, y, xMax - grtMajorLength, y) // draw major graticule
      for (mt <- minorStart to numMinorTics) drawLine(xMax, y + (mt * yMinorInc), xMax - grtMinorLength, y + (mt * yMinorInc))
      ImageText.drawTextOffsetFrom(graphics, xMax - (grtMajorLength + offset + textWidth), y, text, 0) // draw number corresponding to major graticule
      // center vertical
      drawLine(xMiddle - grtMajorLength, y, xMiddle + grtMajorLength, y) // draw major graticule
      for (mt <- minorStart to numMinorTics) drawLine(xMiddle - grtMinorLength, y + (mt * yMinorInc), xMiddle + grtMinorLength, y + (mt * yMinorInc))
      ImageText.drawTextOffsetFrom(graphics, xMiddle + grtMajorLength + offset, y, text, 0) // draw number corresponding to major graticule off to the right
    }

  }

  /**
    * Add graticules to the given image.
    */
  //noinspection ScalaUnusedSymbol
  def addGraticules(image: BufferedImage, translator: IsoImagePlaneTranslator, color: Color): Unit = {

    def x2Pix(xIso: Double) = translator.iso2Pix(xIso, 0).getX.round.toInt

    def y2Pix(yIso: Double) = translator.iso2Pix(0, yIso).getY.round.toInt

    def pix2X(xPix: Double) = translator.pix2Iso(xPix, 0).getX.round.toInt

    def pix2Y(yPix: Double) = translator.pix2Iso(0, yPix).getY.round.toInt

    addGraticules(image, x2Pix, y2Pix, pix2X, pix2Y, color)
  }

  /**
    * Add graticules to the given image, reversing the Y-axis.
    */
  //noinspection ScalaUnusedSymbol
  def addGraticulesNegY(image: BufferedImage, translator: IsoImagePlaneTranslator, color: Color): Unit = {

    def x2Pix(xIso: Double) = translator.iso2Pix(xIso, 0).getX.round.toInt

    def y2Pix(yIso: Double) = translator.iso2Pix(0, -yIso).getY.round.toInt

    def pix2X(xPix: Double) = translator.pix2Iso(xPix, 0).getX.round.toInt

    def pix2Y(yPix: Double) = -translator.pix2Iso(0, yPix).getY.round.toInt

    addGraticules(image, x2Pix, y2Pix, pix2X, pix2Y, color)
  }

}
