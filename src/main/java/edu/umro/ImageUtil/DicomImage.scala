package edu.umro.ImageUtil

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Rectangle
import java.awt.Point

class DicomImage(private val pixelData: IndexedSeq[IndexedSeq[Float]]) {
  def this(attributeList: AttributeList) = this(DicomImage.getPixelData(attributeList))

  val Rows = pixelData.size
  val Columns = pixelData.head.size

  val width = Columns
  val height = Rows

  /**
   * Make a new <code>DicomImage</code> based on a sub-rectangle of this one.  If part of the
   * specified rectangle is out of bounds a cropped version will be used.
   */
  def getSubimage(rectangle: Rectangle): DicomImage = {
    val x = (if (rectangle.getX < 0) 0 else rectangle.getX).toInt
    val y = (if (rectangle.getY < 0) 0 else rectangle.getY).toInt
    val w = (if (rectangle.getX + rectangle.getWidth > width) width - rectangle.getX else rectangle.getWidth).toInt
    val h = (if (rectangle.getY + rectangle.getHeight > height) height - rectangle.getY else rectangle.getHeight).toInt

    val pixDat = (y until h + y).map(row => pixelData(row).drop(x).take(w))
    new DicomImage(pixDat)
  }

  def get(x: Int, y: Int) = pixelData(y)(x)

  /** minimum pixel value. */
  lazy val min = pixelData.map(row => row.min).min

  /** maximum pixel value. */
  lazy val max = pixelData.map(row => row.max).max

  def validPoint(x: Int, y: Int): Boolean = {
    val ok = (x >= 0) && (x < width) && (y >= 0) && (y < height)
    ok
  }

  /**
   * Get the sums of the columns of <code>pixelData</code>
   */
  def columnSums: IndexedSeq[Float] = {
    def colSum(x: Int) = { for (y <- pixelData.indices) yield { get(x, y) } }.sum
    (0 until width).map(x => colSum(x))
  }

  /**
   * Get the sums of the rows of <code>pixelData</code>
   */
  def rowSums = {
    pixelData.map(row => row.sum)
  }

  /**
   * Get a list of the smallest pixel values.
   *
   * @param count: Number of pixel values to get.
   */
  def minPixelValues(count: Int): IndexedSeq[Float] = {
    pixelData.foldLeft(IndexedSeq[Float]())((list, row) => (list ++ row).sorted.take(count))
  }

  /**
   * Get a list of the largest pixel values.
   *
   * @param count: Number of pixel values to get.
   */
  def maxPixelValues(count: Int): IndexedSeq[Float] = {
    pixelData.foldLeft(IndexedSeq[Float]())((list, row) => (row ++ list).sorted.takeRight(count))
  }

  /**
   *  List of offset coordinates for adjacent pixels not including the center pixel.  For
   *  most pixels this is a list of the eight neighbors.
   */
  private val neighbors = {
    val nextTo = Seq(-1, 0, 1)
    for (x <- nextTo; y <- nextTo; if (!((x == 0) && (y == 0)))) yield new Point(x, y)
  }

  /**
   * Use a quick but coarse algorithm to scan the entire image and make a list of the worst pixels.
   */
  private def findWorstPixelsQuickly(count: Int): IndexedSeq[DicomImage.PixelRating] = {
    def ratePixel(x: Int, y: Int): Float = {
      val v = get(x, y)
      val valid = neighbors.map(xy => new Point(x + xy.x, y + xy.y)).filter(xy => validPoint(xy.x, xy.y))
      valid.map(xy => Math.abs(get(xy.x, xy.y) - v)).sum / valid.size
    }

    val ratingList = { for (x <- (0 until width); y <- (0 until height)) yield { new DicomImage.PixelRating(ratePixel(x, y), new Point(x, y)) } }
    val list = ratingList.sortWith((a, b) => (a.rating > b.rating)).take(count)
    list
  }

  /**
   * Find the worst pixels in terms of how severely they differ from their 8 neighbors.
   */
  private def findWorstPixels(sampleSize: Int, maxBadPixels: Int, stdDevMultiple: Double): IndexedSeq[DicomImage.PixelRating] = {
    val worst = findWorstPixelsQuickly(sampleSize)
    val mean = worst.drop(maxBadPixels).map(w => w.rating).sum / (worst.size - maxBadPixels)
    val variance = worst.drop(maxBadPixels).map(w => (w.rating - mean) * (w.rating - mean)).sum / (worst.size - maxBadPixels)
    val stdDev = Math.sqrt(variance)
    val threshold = stdDevMultiple * stdDev
    worst.filter(w => Math.abs(w.rating - mean) > threshold)
  }

  /**
   * Look closely at a potentially bad pixel by comparing it to those in a given radius.  If it
   * is really different, then consider it genuinely bad.
   */
  private def isGenuinelyBadPixel(candidate: DicomImage.PixelRating, stdDevMultiple: Double): Boolean = {
    val radius = 2
    val diameter = (radius * 2) + 1
    val sub = getSubimage(new Rectangle(candidate.point.x - radius, candidate.point.y - radius, diameter, diameter))
    val igbp = sub.findWorstPixels(diameter * diameter, 2, stdDevMultiple).filter(b => (b.point.x == radius) && (b.point.y == radius)).nonEmpty
    igbp
  }

  /**
   * Make a list of all the bad pixels in this image.
   *
   * @param sampleSize: Number of pixels to use as the 'normal' group.  200 is a good value.
   *
   * @param maxBadPixels: Number of bad pixels to assume are bad.  Value can be approximate, 20 is a good value.
   *
   * @param stdDevMultiple: Number to multiply by standard deviation (stdDev * stdDevMultiple) to determine if a pixel is
   * bad.  This value should be greater than 1.0, and the larger it is the more tolerant the algorithm
   * is for allowing pixels to be considered good.   2.0 is a good value.  Note that a value
   * of 1.0 would mark good pixels as bad.
   *
   */
  def identifyBadPixels(sampleSize: Int, maxBadPixels: Int, stdDev: Double): IndexedSeq[DicomImage.PixelRating] = {
    val stdDevMultiple = stdDev + 1
    findWorstPixels(sampleSize, maxBadPixels, stdDevMultiple).filter(bad => isGenuinelyBadPixel(bad, stdDevMultiple))
  }

  /**
   * Given a list of pixel coordinates to correct, create a new <code>DicomImage</code> with
   * each of the listed pixel values replaced by the average of it's neighbors.
   *
   * Note that the correction of a single 1190x1190 image takes about 2 seconds on a XEON 3.6GHz processor.
   */
  def correctBadPixels(badPixelList: IndexedSeq[Point]): DicomImage = {

    case class CorrectedPixel(x: Int, y: Int) {
      val correctedValue: Float = {
        val valueList = neighbors.map(n => new Point(n.x + x, n.y + y)).filter(n => validPoint(n.x, n.y)).map(p => get(p.x, p.y))
        valueList.sum / valueList.size
      }
    }

    val mutablePixelData = pixelData.map(row => row.toArray).toArray
    val corrected = badPixelList.map(b => new CorrectedPixel(b.x, b.y))
    corrected.map(cor => mutablePixelData(cor.y)(cor.x) = cor.correctedValue)
    new DicomImage(mutablePixelData.map(row => row.toIndexedSeq).toIndexedSeq)
  }

  /**
   * Given a flood field, divide every pixel in this image by every pixel in the flood field.
   */
  def biasCorrect(floodImage: DicomImage): DicomImage = {
    val unbiased = (0 until height).map(row => (0 until width).map(col => pixelData(row)(col) / floodImage.pixelData(row)(col)))
    new DicomImage(unbiased)
  }

  /**
   * Construct a <code>BufferedImage</code>, mapping values low to high from black (0) to <code>color</code>.
   *
   * @param rgbColorMap: Colors to use from 0 to 255
   *
   * @param min: minimum pixel value
   *
   * @param max: maximum pixel value
   */
  def toBufferedImage(rgbColorMap: IndexedSeq[Int], min: Float, max: Float): BufferedImage = {

    val ratio = 256 / (max - min)

    /**
     * Given a pixel level, return the RGB color.
     */
    def levelToColor(level: Float): Int = {
      val i = (level * ratio).round.toInt

      i match {
        case _ if (i < 0) => rgbColorMap(0)
        case _ if (i > 255) => rgbColorMap(255)
        case _ => rgbColorMap(i)
      }
    }

    val bufImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    for (x <- (0 until width); y <- (0 until height)) bufImage.setRGB(x, y, levelToColor(get(x, y)))

    bufImage
  }

  def toBufferedImage(rgbColorMap: IndexedSeq[Int]): BufferedImage = toBufferedImage(rgbColorMap, min, max)

  def toBufferedImage(color: Color): BufferedImage = toBufferedImage(ImageUtil.rgbColorMap(color), min, max)

  /**
   * Create a buffered image of this DICOM image using multiple colors to give a deeper color depth of 1276 than the standard 256.
   */
  def toDeepColorBufferedImage(minimumV: Float, maximumV: Float): BufferedImage = {

    val minMax = {
      if (true) {
        (minimumV, maximumV)
      } else {
        //(maximum - minimum).abs.toFloat
        //val bulk = (width * height * 0.99995).round.toInt
        val bulk = (width * height * 0.99999).round.toInt
        def trim(h: Seq[(Float, Int)]): (Float, Float) = {
          println("    bottom: " + h.take(5) + "    top: " + h.takeRight(5)) // TODO rm
          val total = h.map(vc => vc._2).reduce(_ + _)
          0 match {
            case _ if total <= bulk => (h.head._2, h.last._2)
            case _ if h.head._2 < h.last._2 => trim(h.tail)
            case _ => trim(h.dropRight(1))
          }
        }

        val hist = histogram
        println("Starting: bottom: " + hist.take(5) + "    top: " + hist.takeRight(5)) // TODO rm
        trim(hist)
      }
    }

    val range = (minMax._2 - minMax._1).abs
    val minimum = Math.min(minMax._1, minMax._2)

    /**
     * Given a pixel value, return an RGB color
     */
    def p2Color(pixel: Float): Int = {
      val scaledPixel = {
        { (pixel - minimum) / range } match {
          case p if (p < 0) => 0
          case p if (p > 1) => 1
          case p => p
        }
      }

      val a = (1 - scaledPixel) / 0.2
      val X = Math.floor(a).toInt
      val Y = Math.floor(255 * (a - X)).toInt

      val rgb = X match {
        case 0 => (255, Y, 0)
        case 1 => (255 - Y, 255, 0)
        case 2 => (0, 255, Y)
        case 3 => (0, 255 - Y, 255)
        case 4 => (Y, 0, 255)
        case 5 => (255, 0, 255)
        case _ => (255, 0, 255) // pixel outside range
      }
      (rgb._1 << 8) + (rgb._2 << 16) + rgb._3
    }

    val bufImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    for (row <- (0 until height); col <- (0 until width)) bufImage.setRGB(col, row, p2Color(get(col, row)))

    bufImage
  }

  def toDeepColorBufferedImage: BufferedImage = toDeepColorBufferedImage(min, max)

  /**
   * Generate a count of each pixel value.
   *
   * @return List tuples [(pixel value, number of pixels with that value)] sorted by pixel value.
   */
  def histogram: Seq[(Float, Int)] = {
    val list = pixelData.flatten.groupBy(identity).toList.map(g => (g._1, g._2.size)).sortWith((a, b) => a._1 < b._1)
    list
  }
}

object DicomImage {

  case class PixelRating(rating: Float, point: Point) extends Point(point.x, point.y) {
    override def toString = "rating: " + rating + " @ " + point.x + ", " + point.y
  }

  def getPixelData(attributeList: AttributeList): IndexedSeq[IndexedSeq[Float]] = {
    val pixDat = attributeList.getPixelData.getShortValues
    val Rows = attributeList.get(TagFromName.Rows).getIntegerValues()(0)
    val Columns = attributeList.get(TagFromName.Columns).getIntegerValues()(0)

    val pixelDataFloat = (0 until Rows).map(c => pixDat.drop(c * Columns).take(Columns).toIndexedSeq.map(s => (s & 0xffff).toFloat))
    pixelDataFloat
  }
}