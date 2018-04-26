package edu.umro.ImageUtil

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Rectangle

class DicomImage(pixelData: IndexedSeq[IndexedSeq[Float]]) {
  def this(attributeList: AttributeList) = this(DicomImage.getPixelData(attributeList))

  val Rows = pixelData.size
  val Columns = pixelData.head.size

  val width = Columns
  val height = Rows

  def get(x: Int, y: Int) = pixelData(y)(x)

  /** minimum pixel value. */
  lazy val min = pixelData.map(row => row.min).min

  /** maximum pixel value. */
  lazy val max = pixelData.map(row => row.max).max

  case class PixelRating(rating: Float, x: Int, y: Int);

  def validXY(x: Int, y: Int): Boolean = {
    val ok = (x >= 0) && (x < width) && (y >= 0) && (y < height)
    ok
  }

  def findWorstPixels(count: Int): IndexedSeq[PixelRating] = {
    // list of offset coordinates for adjacent pixels
    case class XY(x: Int, y: Int)
    val neighbors = {
      val nextTo = Seq(-1, 0, 1)
      for (x <- nextTo; y <- nextTo; if (!((x == 0) && (y == 0)))) yield new XY(x, y)
    }

    def ratePixel(x: Int, y: Int): Float = {
      val v = get(x, y)
      val valid = neighbors.map(xy => new XY(x + xy.x, y + xy.y)).filter(xy => validXY(xy.x, xy.y))
      valid.map(xy => Math.abs(get(xy.x, xy.y) - v)).sum / valid.size
    }

    val ratingList = { for (x <- (0 until width); y <- (0 until height)) yield { new PixelRating(ratePixel(x, y), x, y) } }
    ratingList.sortWith((a, b) => (a.rating > b.rating)).take(count)
  }

  def identifyBadPixels(sampleSize: Int, maxBadPixels: Int, stdDevMultiple: Double): IndexedSeq[PixelRating] = {
    val worst = findWorstPixels(sampleSize)
    val mean = worst.drop(maxBadPixels).map(w => w.rating).sum / (worst.size - maxBadPixels)
    val variance = worst.drop(maxBadPixels).map(w => (w.rating - mean) * (w.rating - mean)).sum / (worst.size - maxBadPixels)
    val stdDev = Math.sqrt(variance)
    val threshold = stdDevMultiple * stdDev
    worst.filter(w => Math.abs(w.rating - mean) > threshold)
  }

  def toBufferedImage(color: Color): BufferedImage = {

    // calculate a lookup table once
    val rgbTable = {
      val red = color.getRed
      val green = color.getGreen
      val blue = color.getBlue

      def iToRGB(i: Int) = {

        val gg = green * (i / 256.0)

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

    val ratio = 256 / (max - min)

    /**
     * Given a pixel level, return the RGB color.
     */
    def levelToColor(level: Float): Int = {
      val i = (level * ratio).round.toInt

      i match {
        case _ if (i < 0) => rgbTable(0)
        case _ if (i > 255) => rgbTable(255)
        case _ => rgbTable(i)
      }
    }

    val bufImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    for (x <- (0 until width); y <- (0 until height)) bufImage.setRGB(x, y, levelToColor(get(x, y)))

    bufImage
  }

  def subRectangleOf(rectangle: Rectangle): DicomImage = {
    val x = (if (rectangle.getX < 0) 0 else rectangle.getX).toInt
    val y = (if (rectangle.getY < 0) 0 else rectangle.getY).toInt
    val w = (if (rectangle.getX + rectangle.getWidth > width) width - rectangle.getX else rectangle.getWidth).toInt
    val h = (if (rectangle.getY + rectangle.getHeight > height) height - rectangle.getY else rectangle.getHeight).toInt

    // (0 until h).map(row => pixelData(y + row)
    ???
  }
}

object DicomImage {
  def getPixelData(attributeList: AttributeList): IndexedSeq[IndexedSeq[Float]] = {
    val pixDat = attributeList.getPixelData.getShortValues
    val Rows = attributeList.get(TagFromName.Rows).getIntegerValues()(0)
    val Columns = attributeList.get(TagFromName.Columns).getIntegerValues()(0)

    val pixelDataFloat = (0 until Rows).map(c => pixDat.drop(c * Columns).take(Columns).toIndexedSeq.map(s => (s & 0xffff).toFloat))
    pixelDataFloat
  }
}