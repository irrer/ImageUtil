package edu.umro.ImageUtil

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Rectangle
import java.awt.Point
import edu.umro.ScalaUtil.Trace

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
    val nextTo = Seq(-2, -1, 0, 1, 2)
    for (x <- nextTo; y <- nextTo; if (!((x == 0) && (y == 0)))) yield new Point(x, y)
  }

  /**
   * Use a quick but coarse algorithm to scan the entire image and make a list of the worst pixels.
   */
  private def findWorstPixelsQuickly1(count: Int): IndexedSeq[DicomImage.PixelRating] = {
    def ratePixel(x: Int, y: Int): Float = {
      val v = get(x, y)
      if (v > 60000)
        println("Big: " + v)
      val valid = neighbors.map(xy => new Point(x + xy.x, y + xy.y)).filter(xy => validPoint(xy.x, xy.y))
      val rating = valid.map(xy => Math.abs(get(xy.x, xy.y) - v)).sum / valid.size
      rating
    }

    val ratingList = { for (x <- (0 until width); y <- (0 until height)) yield { new DicomImage.PixelRating(ratePixel(x, y), new Point(x, y)) } }
    val list = ratingList.sortWith((a, b) => (a.rating > b.rating)).take(count)
    list
  }

  /**
   * Use a quick but coarse algorithm to scan the entire image and make a list of the worst pixels.
   *
   * @param maxBadPix: restrict count of bad pixels to this many
   *
   * @param radius: Distance to the farthest pixel for comparison.  For example, a
   * value of 3 would mean that a (3*2+1)*(3*2+1) = 7*7 pixel square would be used.
   */
  private def findWorstPixelsQuickly(maxBadPix: Int, radius: Int): Seq[DicomImage.PixelRating] = {

    val diam = radius * 2 + 1
    val area = diam * diam

    def startAt(d: Int, limit: Int): Int = {
      if (d <= radius) 0
      else Math.min(d - radius, limit - diam)
    }

    case class Column(x: Int, rowList: IndexedSeq[Float]) {
      def this(x: Int) = this(x, {
        val s = startAt(x, width)
        (0 until height).map(y => pixelData(y).drop(s).take(diam).sum)
      })

      /**
       * Get the next column by subtracting the first X value of each row and adding the next.
       */
      def next: Column = {
        if (x >= (width - diam)) {
          this // going off the end
        } else {
          if (startAt(x, width) == startAt(x + 1, width)) {
            new Column(x + 1, rowList)
          } else {
            val xm1 = x - 1
            val row = (0 until height).map(y => {
              rowList(y) - pixelData(y)(x - radius) + pixelData(y)(x - radius + diam)
            })
            val newCol = new Column(x + 1, row)
            newCol
          }
        }
      }
    }

    case class Row(y: Int, column: Column, total: Float) {
      def this(y: Int, column: Column) = this(y, column, column.rowList.drop(startAt(y, height)).take(diam).sum)
      val avg = total / area
      /**
       * Rate the 'badness' of this pixel.  The higher the number, the badder it is.
       */
      def rate(v: Float) = {
        ((avg - v) / avg).abs
      }

      def next: Row = {
        if (startAt(y, height) == startAt(y + 1, height))
          new Row(y + 1, column, total)
        else {
          // subtract the first and add the next
          new Row(y + 1, column, total - column.rowList(y - radius) + column.rowList(y + radius + 1))
        }
      }
    }

    def evalCol(col: Column, pixRating: Seq[DicomImage.PixelRating], x: Int): (Column, Seq[DicomImage.PixelRating]) = {

      val row = new Row(0, col)
      val score = (row.avg - get(x, 0)) / row.avg

      (0 until height).map(y => new DicomImage.PixelRating((row.avg - get(x, y)) / row.avg, new Point(x, y)))

      def evalRow(row: Row, pixRtg: Seq[DicomImage.PixelRating], y: Int) = {

        val pixRat = new DicomImage.PixelRating(row.rate(get(x, y)), new Point(x, y))

        (row.next, pixRtg :+ pixRat)
      }

      val pixRatList = (0 until height).foldLeft(row, pixRating)((rp, y) => evalRow(rp._1, rp._2, y))._2

      val pixRatListSortedAndTrimmed = pixRatList.sortWith((a, b) => a.rating > b.rating).take(maxBadPix)

      (col.next, pixRatListSortedAndTrimmed)
    }

    val result = (0 until width).foldLeft(new Column(0), Seq[DicomImage.PixelRating]())((cr, x) => evalCol(cr._1, cr._2, x))
    result._2
  }

  /**
   * Public function wrapper to support testing.
   */
  def testFindWorstPixelsQuickly(maxBadPix: Int, radius: Int): Seq[DicomImage.PixelRating] = findWorstPixelsQuickly(maxBadPix, radius)

  /**
   * Find the worst pixels in terms of how severely they differ from their neighbors.
   */
  private def findWorstPixels(sampleSize: Int, maxBadPixels: Int, stdDevMultiple: Double): Seq[DicomImage.PixelRating] = {
    val worst = findWorstPixelsQuickly(sampleSize, 5)
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
  def identifyBadPixels1(sampleSize: Int, maxBadPixels: Int, stdDev: Double): Seq[DicomImage.PixelRating] = {
    val stdDevMultiple = stdDev + 1
    findWorstPixels(sampleSize, maxBadPixels, stdDevMultiple).filter(bad => isGenuinelyBadPixel(bad, stdDevMultiple))
  }

  private def ratePixelByStdDev(xc: Int, yc: Int): DicomImage.PixelRating = {

    val radius = 2
    val diam = (radius * 2) + 1

    val maxX = width - diam
    val x = {
      val xx = xc - radius
      if (xx < 0) 0
      else if (xx > maxX) maxX
      else xx
    }

    val maxY = height - diam
    val y = {
      val yy = yc - radius
      if (yy < 0) 0
      else if (yy > maxY) maxY
      else yy
    }

    val pixels: IndexedSeq[Float] = {
      try {
        (y until (y + diam)).map(yy => pixelData(yy).drop(x).take(diam)).flatten
      } catch {
        case t: Throwable => {
          println("out of bounds")
          IndexedSeq[Float](1, 2, 3, 4, 5)
        }
      }
    }

    val pixOfIntrst = get(xc, yc)
    val total = pixels.sum
    val mean = total / pixels.size

    def std(v: Float) = {
      val vv = v - mean
      vv * vv
    }

    val variance = pixels.map(std).sum / pixels.size

    val deviation: Float =
      if (variance == 0) {
        0
      } else {
        val stdDev = Math.sqrt(variance)
        val deviation = (pixOfIntrst - mean).abs / stdDev
        if (deviation.toString.replaceAllLiterally("E-", "").toLowerCase.matches(".*[a-z].*"))
          println("bad number")
        if (deviation < 0)
          println("hey <")
        deviation.toFloat
      }
    new DicomImage.PixelRating(deviation, new Point(xc, yc))
  }

  private def getMostOfHistogram(size: Int, hist: Seq[DicomImage.HistPoint]): (Int, Int) = {

    def remove(h: Seq[DicomImage.HistPoint], s: Int, lo: Int, hi: Int): (Seq[DicomImage.HistPoint], (Int, Int)) = {
      val hc = h.head.count
      val lc = h.last.count
      if ((s <= size) || (hc > 2 && lc > 2)) (h, (lo, hi))
      else {
        if (hc < lc) {
          remove(h.tail, s - hc, lo + 1, hi)
        } else
          remove(h.init, s - lc, lo, hi - 1)
      }
    }
    val culled = remove(hist, width * height, 0, hist.size - 1)
    culled._2
  }

  private def identifyOutlierPixels: DicomImage.BadPixelSet = {

    if (true) {
      val max10 = { for (y <- 0 until height; x <- 0 until width) yield { new DicomImage.PixelRating(get(x, y), new Point(x, y)) } }.sortWith((a, b) => a.rating < b.rating).takeRight(10)
      println("max10:\n" + max10.mkString("\n    "))
    }

    val percentBulk = 1.0 // TODO make into configuration parameter
    val percentBoundary = 10.0 // TODO make into configuration parameter
    val hist = histogram
    val mostRange = getMostOfHistogram(((1 - percentBulk / 100) * width * height).round.toInt, hist)

    if (true) {
      println("mostRange lo: " + hist(mostRange._1))
      println("mostRange hi: " + hist(mostRange._2))
    }

    val allPixels = pixelData.flatten

    def allPixelsOutsideRange(lo: Double, hi: Double): Seq[Point] = {
      def outside(v: Float) = { (v > hi) || (v < lo) }
      for (y <- 0 until height; x <- 0 until width; if outside(get(x, y))) yield { new Point(x, y) }
    }

    val maxAllowed = hist(mostRange._2).value * (1 + percentBoundary / 100.0)
    val minAllowed = hist(mostRange._1).value * (1 - percentBoundary / 100.0)

    val list = allPixelsOutsideRange(minAllowed, maxAllowed)

    if (true) {
      println("minAllowed: " + minAllowed)
      println("maxAllowed: " + maxAllowed)
      val outies = list.map(p => new DicomImage.PixelRating(get(p.x, p.y), p)).sortWith((a, b) => a.rating < b.rating)
      println("outies:\n" + outies.mkString("\n    "))
    }

    new DicomImage.BadPixelSet(list.map(p => new DicomImage.PixelRating(10, p)), minAllowed.toFloat, maxAllowed.toFloat)
  }

  lazy val mean = pixelData.flatten.sum / (width * height) // TODO probably not needed

  lazy val stdDev = { // TODO probably not needed
    val avg = mean

    def sd(v: Float) = {
      val vv = v - avg
      vv * vv
    }
    val variance = pixelData.flatten.map(v => sd(v)).sum / (width * height)
    Math.sqrt(variance)
  }

  def identifyBadPixels(sampleSize: Int, maxBadPixels: Int, stdDev: Double): DicomImage.BadPixelSet = {
    val start = System.currentTimeMillis
    val all = for (x <- 0 until width; y <- 0 until height) yield { ratePixelByStdDev(x, y) }
    val elapsed = System.currentTimeMillis - start
    println("elapsed: " + elapsed)
    val unique = all.map(pr => pr.rating).distinct
    //println("unique: " + unique.mkString("\n    ", "\n    ", "\n    "))
    println("unique.size: " + unique.size)
    val allSorted = all.toList.sortWith((a, b) => a.rating < b.rating)
    val worst = allSorted.takeRight(maxBadPixels).reverse.filter(pr => pr.rating > (stdDev + 1))

    val outliers = identifyOutlierPixels
    val distinctOutliers = outliers.list.filter(p => worst.find(b => b.point == p).isEmpty).map(p => new DicomImage.PixelRating(10, p))
    val list = worst.toIndexedSeq ++ distinctOutliers
    new DicomImage.BadPixelSet(list, outliers.minBound, outliers.maxBound)
  }

  /**
   * Given a list of pixel coordinates to correct, create a new <code>DicomImage</code> with
   * each of the listed pixel values replaced by the average of it's neighbors.
   *
   * Note that the correction of a single 1190x1190 image takes about 2 seconds on a XEON 3.6GHz processor.
   */
  def correctBadPixels(badPixels: DicomImage.BadPixelSet): DicomImage = {

    case class CorrectedPixel(x: Int, y: Int) {
      private val valueList = neighbors.map(n => new Point(n.x + x, n.y + y)).filter(n => validPoint(n.x, n.y)).map(p => get(p.x, p.y))
      val correctedValue: Float = {
        val avg = valueList.sum / valueList.size
        if (avg <= badPixels.minBound) badPixels.minBound
        else if (avg >= badPixels.maxBound) badPixels.maxBound
        else avg
      }
      // println("CorrectedPixel  x: " + x + "   y: " + y + "     old value: " + get(x, y) + "     corrected value: " + correctedValue + "    neighVal: " + valueList.mkString("  "))
    }

    Math.max(2.4, 3.4)

    val mutablePixelData = pixelData.map(row => row.toArray).toArray
    val corrected = badPixels.list.map(b => new CorrectedPixel(b.x, b.y))
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

    val range = (maximumV - minimumV).abs

    /**
     * Given a pixel value, return an RGB color
     */
    def p2Color(pixel: Float): Int = {
      val scaledPixel = {
        { (pixel - min) / range } match {
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
  def histogram: Seq[DicomImage.HistPoint] = {
    val list = pixelData.flatten.groupBy(identity).toList.map(g => new DicomImage.HistPoint(g._1, g._2.size)).sortWith((a, b) => a.value < b.value)
    list
  }

  def binnedHistogram(size: Int): Seq[DicomImage.HistPoint] = {
    val incr = (max - min) / size
    def vToBin(v: Float): Int = ((v - min) / incr).floor.toInt

    def binToV(bs: (Int, Int)): DicomImage.HistPoint = new DicomImage.HistPoint(((bs._1 * incr) + min), bs._2)
    val list = pixelData.flatten.groupBy(v => vToBin(v)).toList.map(g => (g._1, g._2.size)).sortWith((a, b) => a._1 < b._1).map(bs => binToV(bs))
    list
  }

}

object DicomImage {

  case class PixelRating(rating: Float, point: Point) extends Point(point.x, point.y) {
    override def toString = "rating: " + rating + " @ " + point.x + ", " + point.y
  }

  case class HistPoint(value: Float, count: Int);

  case class BadPixelSet(list: Seq[PixelRating], minBound: Float, maxBound: Float);

  def getPixelData(attributeList: AttributeList): IndexedSeq[IndexedSeq[Float]] = {
    val pixDat = attributeList.getPixelData.getShortValues
    val Rows = attributeList.get(TagFromName.Rows).getIntegerValues()(0)
    val Columns = attributeList.get(TagFromName.Columns).getIntegerValues()(0)

    val pixelDataFloat = (0 until Rows).map(c => pixDat.drop(c * Columns).take(Columns).toIndexedSeq.map(s => (s & 0xffff).toFloat))
    pixelDataFloat
  }
}