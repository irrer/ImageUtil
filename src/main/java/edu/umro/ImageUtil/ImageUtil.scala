/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.umro.ImageUtil

import org.opensourcephysics.numerics.CubicSpline

import java.awt.geom.Point2D
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, Graphics2D, RenderingHints}
import java.io.File
import java.security.InvalidParameterException
import javax.imageio.ImageIO
import scala.annotation.tailrec

object ImageUtil {

  /**
    * Get the graphics for the image, setting some preferred defaults.
    */
  def getGraphics(bufferedImage: BufferedImage): Graphics2D = {
    val graphics = bufferedImage.getGraphics.asInstanceOf[Graphics2D]
    graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
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
    * Solid line stroke / style
    */
  val solidLine = new BasicStroke

  /**
    * Set line drawing to solid.
    */
  //noinspection ScalaUnusedSymbol
  def setSolidLine(graphics: Graphics2D): Unit = graphics.setStroke(solidLine)

  /**
    * Write the given text on the image near the given pixel.  Offset it in X and Y so that it does not obscure the
    * pixel, and use an offset that will still be inside the image by putting it roughly between the pixel and the
    * center of the image.
    */
  def annotatePixel(bufferedImage: BufferedImage, x: Double, y: Double, text: String, encircle: Boolean): Unit = {
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
    for (x <- 0 until original.getWidth; y <- 0 until original.getHeight) {
      val rgb = original.getRGB(x, y)
      for (xm <- 0 until factor; ym <- 0 until factor) magnified.setRGB(x * factor + xm, y * factor + ym, rgb)
    }
    magnified
  }

  /**
    * Given an image, mirror it vertically.  Pixels on the top will be on the bottom, and
    * vice-versa.  Pixels on the left will stay on the left.  Pixels on the right will
    * stay on the right.
    */
  def mirrorVertically(original: BufferedImage): BufferedImage = {
    val width = original.getWidth
    val height = original.getHeight
    val mirrored = new BufferedImage(width, height, original.getType)
    for (x <- 0 until width; y <- 0 until height) {
      mirrored.setRGB(x, height - y - 1, original.getRGB(x, y))
    }
    mirrored
  }

  /**
    * Given an image, mirror it horizontally.  Pixels on the left will be on the right, and
    * vice-versa.  Pixels on the top will stay on the top.  Pixels on the bottom will
    * stay on the bottom.
    */
  def mirrorHorizontally(original: BufferedImage): BufferedImage = {
    val width = original.getWidth
    val height = original.getHeight
    val mirrored = new BufferedImage(width, height, original.getType)

    for (x <- 0 until width; y <- 0 until height) {
      mirrored.setRGB(width - x - 1, y, original.getRGB(x, y))
    }
    mirrored
  }

  /**
    * Given an image, rotate it clockwise by 90 degrees.
    */
  def rotate90(original: BufferedImage): BufferedImage = {
    val rotated = new BufferedImage(original.getHeight, original.getWidth, original.getType)
    val origWidth = original.getWidth
    val origHeight = original.getHeight
    for (x <- 0 until origWidth; y <- 0 until original.getHeight) {
      rotated.setRGB(origHeight - 1 - y, x, original.getRGB(x, y))
    }
    rotated
  }

  /**
    * Given an image, rotate it clockwise by 180 degrees.
    */
  def rotate180(original: BufferedImage): BufferedImage = {
    rotate90(rotate90(original))
  }

  /**
    * Given an image, rotate it clockwise by 270 degrees.
    */
  def rotate270(original: BufferedImage): BufferedImage = {
    rotate90(rotate180(original))
  }

  /**
    * Find the center of mass of a set of points, indexed starting from 0.
    */
  def centerOfMass(massList: IndexedSeq[Float]): Float = {
    if (massList.isEmpty)
      0.toFloat
    else {
      val massSum = massList.map(f => f.toDouble).sum
      val sumList = massList.indices.map(i => i * massList(i).toDouble)
      (sumList.sum / massSum).toFloat
    }
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
          case _ if i < 0   => 0
          case _ if i > 255 => 255
          case i            => i
        }
        lev << shift
      }

      toColor(red, i, 16) + toColor(green, i, 8) + toColor(blue, i, 0)
    }

    (0 until 256).map(i => iToRGB(i))
  }

  /**
    * Make the color map brighter or dimmer by multiplying colors by the given factor.  If
    * the factor is greater than one it will make the map brighter, less than
    * one will make it dimmer.  Negative numbers will make it all black.  A factor of 255 or
    * greater will keep black pixels black and change all others to maximum brightness.
    */
  //noinspection ScalaUnusedSymbol
  def adjustBrightness(factor: Double, colorMap: IndexedSeq[Int]): IndexedSeq[Int] = {
    def adj(c: Int): Int = Math.max(0, Math.min((c * factor).round.toInt, 255))

    def adjust(clr: Int): Int = {
      val c = new Color(clr)
      val c2 = new Color(adj(c.getRed), adj(c.getGreen), adj(c.getBlue))
      c2.getRGB
    }
    colorMap.map(clr => adjust(clr))
  }

  /**
    * Set the minimum pixel brightness.
    */
  //noinspection ScalaUnusedSymbol
  def setColorMapFloor(min: Int, colorMap: IndexedSeq[Int]): IndexedSeq[Int] = {

    def brightness(clr: Int): Int = {
      val c = new Color(clr)
      ((c.getRed + c.getGreen + c.getBlue) / 3.0).floor.toInt
    }

    def minColor = colorMap.minBy(c => (brightness(c) - min).abs)

    def adjust(clr: Int): Int = {
      if (brightness(clr) < min) minColor else clr
    }
    colorMap.map(clr => adjust(clr))
  }

  /**
    * Given a range, return a list of major graticule positions within that
    * range that will make a user friendly display.
    *
    * @param min: Minimum value of range to be displayed
    *
    * @param max: Maximum value of range to be displayed
    *
    * @param maxCount: Maximum number of graticules to be returned.  Must be greater than 0.
    */
  def graticule(min: Double, max: Double, maxCount: Int): Seq[Double] = {
    val range = (max - min).abs
    val majorCandidates: Seq[Double] = Seq(1, 5, 10, 20, 25)
    val multiple = Math.pow(10.0, Math.log10(range).floor - 2)

    @tailrec
    def getIncrement(mult: Double): Double = {
      def works(grat: Double): Boolean = {
        maxCount >= (range / grat)
      }
      majorCandidates.map(c => c * mult).find(c => works(c)) match {
        case Some(c) => c
        case _       => getIncrement(mult * 10)
      }
    }

    val increment = getIncrement(multiple)

    val lo = Math.min(min, max)
    val hi = Math.max(min, max)

    val begin = ((lo - (lo % increment)) / increment).round.toInt
    val end = ((hi - (hi % increment)) / increment).round.toInt
    val gratList = for (g <- (begin - 2) to (end + 2); if ((g * increment) >= lo) && ((g * increment) <= hi)) yield g * increment
    if (min < max) gratList else gratList.reverse
  }

  /**
    * Write a buffered image as a PNG file.
    */
  def writePngFile(image: BufferedImage, file: File): Boolean = ImageIO.write(image, "png", file)

  /**
    * Find the maximum point in a profile by fitting a cubic spline over it and then searching.  It is
    * assumed that the profile has exactly one local maximum.  (ie: one hump, not multiple).
    *
    * @param xList: List of X positions of data.  Each value must be unique.
    *
    * @param yList: List of Y values corresponding to X values.  The profile of these values is examined.
    */
  def profileMaxCubic(xList: Array[Double], yList: Array[Double]): Double = {
    val cubic = new CubicSpline(xList, yList)
    val maxIteration = 20
    val divs = 5
    case class Pt(x: Double) {
      lazy val y: Double = cubic.evaluate(x)
    }

    @tailrec
    def search(iter: Int, a: Pt, b: Pt): Double = {
      if (iter < 1) Seq(a, b).maxBy(_.y).x
      else {
        val incr = (a.x - b.x).abs / divs
        val minX = Math.min(a.x, b.x)
        val between = (1 until divs).map(i => Pt(minX + (i * incr)))
        val all = (Seq(a, b) ++ between).sortBy(_.x)
        val best = all.maxBy(pt => pt.y)
        search(iter - 1, Pt(best.x - incr), Pt(best.x + incr))
      }
    }

    val minX = xList.min
    val maxX = xList.max
    val ext = (maxX - minX) / xList.length

    val loX = minX - ext
    val hiX = maxX + ext
    search(maxIteration, Pt(loX), Pt(hiX))
  }

  /**
    * Calculate standard deviation.
    */
  def stdDev(list: Seq[Float]): Double = {
    val mean = list.sum / list.size
    val sumSq = list.map(d => (d - mean) * (d - mean)).sum
    val variance = sumSq / list.size
    val sd = Math.sqrt(variance)
    sd
  }

  /**
    * Set the graphics line thickness.
    */
  //noinspection ScalaUnusedSymbol
  def setLineThickness(graphics: Graphics2D, lineThickness: Double): Unit = {
    graphics.setStroke(new BasicStroke(lineThickness.toFloat))

  }

  /**
    * Clone a buffered image.
    * @param bi Original.
    * @return Copy of original.
    */
  //noinspection ScalaUnusedSymbol
  def deepCopy(bi: BufferedImage): BufferedImage = {
    val cm = bi.getColorModel
    val isAlphaPreMultiplied = cm.isAlphaPremultiplied
    val raster = bi.copyData(bi.getRaster.createCompatibleWritableRaster())
    new BufferedImage(cm, raster, isAlphaPreMultiplied, null)
  }

}
