
package edu.umro.ImageUtil

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Rectangle
import java.awt.Point
import edu.umro.ScalaUtil.Trace
import java.security.InvalidParameterException
import java.awt.geom.Point2D
import javax.vecmath.Point2i
import java.awt.geom.Rectangle2D

class DicomImage(val pixelData: IndexedSeq[IndexedSeq[Float]]) {
  def this(attributeList: AttributeList) = this(DicomImage.getPixelData(attributeList))

  val Rows = pixelData.size
  val Columns = {
    if (pixelData.size == 0) 0
    else pixelData.head.size
  }

  val width = Columns
  val height = Rows

  def getSubArray(rectangle: Rectangle): IndexedSeq[IndexedSeq[Float]] = {
    val x = (if (rectangle.getX < 0) 0 else rectangle.getX).toInt
    val y = (if (rectangle.getY < 0) 0 else rectangle.getY).toInt
    val w = (if (rectangle.getX + rectangle.getWidth > width) width - rectangle.getX else rectangle.getWidth).toInt
    val h = (if (rectangle.getY + rectangle.getHeight > height) height - rectangle.getY else rectangle.getHeight).toInt

    val pixDat = (y until h + y).map(row => pixelData(row).drop(x).take(w))
    pixDat
  }

  /**
   * Make a new <code>DicomImage</code> based on a sub-rectangle of this one.  If part of the
   * specified rectangle is out of bounds a cropped version will be used.
   */
  def getSubimage(rectangle: Rectangle): DicomImage = {
    new DicomImage(getSubArray(rectangle))
  }

  def get(x: Int, y: Int) = {
    try pixelData(y)(x)
    catch {
      case t: Throwable => {
        0.toFloat
      }
    }
  }

  /** minimum pixel value. */
  lazy val minPixelValue = pixelData.map(row => row.min).min

  /** maximum pixel value. */
  lazy val maxPixelValue = pixelData.map(row => row.max).max

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
   * Get the sum of all of the pixels.
   */
  def sum = rowSums.sum

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
   *
   * Note: As quick as this is, it takes about 7.5 seconds to process an 1190x1190 image.
   *
   * @param maxBadPix: restrict count of bad pixels to this many
   *
   * @param radius: Distance to the farthest pixel for comparison.  For example, a
   * value of 3 would mean that a (3*2+1)*(3*2+1) = 7*7 pixel square would be used.
   */
  private def findWorstPixelsQuickly(maxBadPix: Int, radius: Int, BadPixelMinimumDeviation_CU: Double): Seq[DicomImage.PixelRating] = {

    val diam = radius * 2 + 1
    val area = diam * diam

    class WorstPixels {
      val buf = scala.collection.mutable.ArrayBuffer(new DicomImage.PixelRating(-1, 0, 0))

      var smallestRating: Float = buf.head.rating
      var smallestIndex = 0

      def shouldPut(value: Float) = (value > smallestRating) || (buf.size < maxBadPix)

      def put(rating: Float, x: Int, y: Int) = {
        if (buf.size < maxBadPix) {
          buf += new DicomImage.PixelRating(rating, x, y)
        } else {
          buf(smallestIndex) = new DicomImage.PixelRating(rating, x, y)
          smallestRating = buf.head.rating
          smallestIndex = 0
          buf.indices.map(i => if (buf(i).rating < smallestRating) {
            smallestRating = buf(i).rating
            smallestIndex = i
          })
        }
      }
    }

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
        if (x >= (width - 1)) {
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
       * Rate the 'badness' of this pixel.  The higher the number, the worse it is.
       */
      def rate(v: Float) = {
        val diffAvg = (avg - v).abs // absolute difference from average
        if (diffAvg <= BadPixelMinimumDeviation_CU) 0
        else diffAvg / avg
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

    val worstPixels = new WorstPixels

    def evalCol(col: Column): Unit = {
      val row = new Row(0, col)

      def evalRow(row: Row, y: Int): Row = {
        val rating = row.rate(get(col.x, y))
        if (worstPixels.shouldPut(rating))
          worstPixels.put(rating, col.x, y)
        row.next
      }

      (0 until height).foldLeft(row)((r, y) => evalRow(r, y))
    }

    (0 until width).foldLeft(new Column(0))((col, x) => {
      evalCol(col)
      col.next
    })
    DicomImage.sortPixelRating(worstPixels.buf).take(maxBadPix)
  }

  private def scoreWithStandardDeviation(badList: Seq[DicomImage.PixelRating], radius: Int): Seq[DicomImage.PixelRating] = {

    val diam = radius * 2 + 1

    def startAt(d: Int, limit: Int): Int = {
      if (d <= radius) 0
      else Math.min(d - radius, limit - diam)
    }

    val badSet = badList.map(b => (b.x, b.y)).toSet

    def isGood(x: Int, y: Int) = {
      val good = !badSet.contains((x, y))
      !badSet.contains((x, y))
    }

    /**
     * Given a bad pixel, determine how many (multiples of) standard deviation it is from its neighbors.
     */
    def stdDev(bp: DicomImage.PixelRating, radius: Int): DicomImage.PixelRating = {
      val xs = startAt(bp.x, width)
      val ys = startAt(bp.y, height)
      val pixelValue = get(bp.x, bp.y)

      val goodList = for (x <- xs until (xs + diam); y <- ys until (ys + diam); if isGood(x, y)) yield (get(x, y))

      val mean = goodList.sum / goodList.size

      def stdOf(v: Float) = {
        val vm = v - mean
        val s = vm * vm
        s
      }

      val variance = goodList.map(v => stdOf(v)).sum / Math.max(goodList.size, 1)
      val stdDev = Math.sqrt(variance).toFloat
      val rating = ((pixelValue - mean) / stdDev).abs

      new DicomImage.PixelRating(rating, bp.x, bp.y)
    }

    badList.map(bp => stdDev(bp, radius))
  }

  /**
   * Public function wrapper to support testing.
   */
  def testFindWorstPixelsQuickly(maxBadPix: Int, radius: Int, BadPixelMinimumDeviation_CU: Double): Seq[DicomImage.PixelRating] = findWorstPixelsQuickly(maxBadPix, radius, BadPixelMinimumDeviation_CU)

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

  private def identifyPixelsWithAbnormallyLargeValues(maxBadPixels: Int, BadPixelMaximumPercentChange: Double, badList: Seq[DicomImage.PixelRating]): Seq[DicomImage.PixelRating] = {

    val mult = 1 + (BadPixelMaximumPercentChange / 100)
    val knownBad = badList.map(pr => (pr.x, pr.y)).toSet
    def isGood(pr: DicomImage.PixelRating): Boolean = !knownBad.contains(pr.x, pr.y)

    // get the largest pixels that are not on the bad list
    val big = { for (x <- 0 until width; y <- 0 until height) yield (DicomImage.PixelRating(pixelData(y)(x), x, y)) }.filter(pr => isGood(pr)).sortBy(_.rating).takeRight(maxBadPixels * 2)

    def closeTo(a: Int, b: Int): Boolean = {
      (big(a).rating * mult) >= big(b).rating
    }

    val good = (0 until big.size - 1).map(i => i).takeWhile(i => closeTo(i, i + 1))

    val abnormal =
      if (good.isEmpty)
        Seq[DicomImage.PixelRating]()
      else
        big.drop(good.last + 1)

    abnormal
  }

  //  private def identifyLargeOutlierPixels(maxBadPixels: Int, BadPixelMaximumPercentChange: Double, sortedStdDeviationBadList: Seq[DicomImage.PixelRating]): Seq[DicomImage.PixelRating] = {
  //    val mult = 1 + (BadPixelMaximumPercentChange / 100)
  //    def isBad(i: Int) = {
  //      (histogram(i).count < 3) &&
  //        (histogram(i + 1).count < 3) &&
  //        (histogram(i).value * mult) < histogram(i + 1).value
  //    }
  //
  //    val cutoffIndex = histogram.tail.indices.find(i => isBad(i))
  //
  //    if (cutoffIndex.isDefined) {
  //      val badIndex = cutoffIndex.get + 1
  //      val badValue = histogram(badIndex).value
  //
  //      val outlierList = { for (x <- 0 until width; y <- 0 until height; if get(x, y) >= badValue) yield (DicomImage.PixelRating(pixelData(y)(x), x, y)) }.sortBy(_.rating).takeRight(maxBadPixels * 2)
  //      val knownBadSet = sortedStdDeviationBadList.map(pr => (pr.x, pr.y)).toSet
  //      def notKnown(pr: DicomImage.PixelRating) = !knownBadSet.contains((pr.x, pr.y))
  //      outlierList.filter(o => notKnown(o))
  //
  //    } else Seq[DicomImage.PixelRating]()
  //  }

  def identifyBadPixels(maxBadPixels: Int, stdDev: Double, BadPixelMaximumPercentChange: Double, radius: Int, BadPixelMinimumDeviation_CU: Double): Seq[DicomImage.PixelRating] = {

    /**
     * Show the pixels in the immediate area for debugging.
     */
    def pr2s(pr: DicomImage.PixelRating): Unit = {
      println("\n---- " + pr.x + ", " + pr.y + "  rating: " + pr.rating + "  value: " + get(pr.x, pr.y))
      val rad = 8
      val diam = rad * 2 + 1
      val area = getSubimage(new Rectangle(pr.x - rad, pr.y - rad, diam, diam))

      for (x <- 0 until area.width) {
        print("\n    ")
        for (y <- 0 until area.height) {
          print(area.get(x, y).round.toInt.formatted("%7d"))
        }
      }
      println
    }

    val coarseBadList = findWorstPixelsQuickly(maxBadPixels * 4, radius, BadPixelMinimumDeviation_CU)

    val limit = stdDev + 1
    val stdDeviationBadListUnsorted = scoreWithStandardDeviation(coarseBadList, radius).filter(bp => bp.rating >= limit)
    val sortedStdDeviationBadList = stdDeviationBadListUnsorted.sortBy(_.rating).takeRight(maxBadPixels).reverse

    //val outlierList = identifyLargeOutlierPixels(maxBadPixels, BadPixelMaximumPercentChange, sortedStdDeviationBadList)

    // only add bad pixels that were not previously discovered
    // val outlierNew = outlierList.filter(o => stdDeviationBadListUnsorted.find(w => ((w.x == o.x) && (w.y == o.y))).isEmpty)

    //(sortedStdDeviationBadList ++ outlierList).sortBy(_.rating).reverse
    sortedStdDeviationBadList
  }

  def correctBadPixels(badList: Seq[DicomImage.PixelRating], radius: Int): DicomImage = {
    val diam = radius * 2 + 1

    def startAt(d: Int, limit: Int): Int = {
      if (d <= radius) 0
      else Math.min(d - radius, limit - diam)
    }

    val badSet = badList.map(b => (b.x, b.y)).toSet

    def isGood(x: Int, y: Int) = { !badSet.contains((x, y)) }

    val mutablePixelData = pixelData.map(row => row.toArray).toArray

    def correct(bp: DicomImage.PixelRating): Unit = {
      val xs = startAt(bp.x, width)
      val ys = startAt(bp.y, height)
      val goodList = for (x <- xs until (xs + diam); y <- ys until (ys + diam); if isGood(x, y)) yield get(x, y)
      val mean = goodList.sum / goodList.size
      mutablePixelData(bp.y)(bp.x) = mean
    }

    badList.map(bp => correct(bp))
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
      val i = ((level - min) * ratio).round.toInt

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

  def toBufferedImage(rgbColorMap: IndexedSeq[Int]): BufferedImage = toBufferedImage(rgbColorMap, minPixelValue, maxPixelValue)

  def toBufferedImage(color: Color): BufferedImage = toBufferedImage(ImageUtil.rgbColorMap(color), minPixelValue, maxPixelValue)

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
        { (pixel - minPixelValue) / range } match {
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

  /**
   * Make a deep-color buffered image of the image.
   *
   * @param percentToDrop: The percent of pixels that will be dropped from the high and low end of the
   * range of values to avoid letting a few bad pixels with extremely high or low values to expand the
   * pixel range that most of the image contrast is lost.  To ignore this effect and use the full pixel
   * range, use a value of 0.
   *
   * If a value of 0.5 is given, then 0.25 percent of the lowest valued pixels and 0.25 percent of the
   * highest valued pixels will be dropped, and instead of being given their true color will be given the
   * color for the lowest or highest value respectively.
   */
  def toDeepColorBufferedImage(percentToDrop: Double): BufferedImage = {
    val numDrop = ((Rows * Columns) * ((percentToDrop / 2) / 100.0)).round.toInt
    def trim(total: Int, hist: Seq[DicomImage.HistPoint]): Seq[DicomImage.HistPoint] = {
      if ((total < numDrop) && hist.nonEmpty)
        trim(total + hist.head.count, hist.tail)
      else hist
    }
    if (histogram.size >= 1276) {
      val dropLo = trim(0, histogram)
      val minPix = if (dropLo.isEmpty) minPixelValue else dropLo.head.value
      val dropHi = trim(0, dropLo.reverse)
      val maxPix = if (dropHi.isEmpty) maxPixelValue else dropHi.head.value

      toDeepColorBufferedImage(minPix, maxPix)
    } else toDeepColorBufferedImage(minPixelValue, maxPixelValue)
  }

  /**
   * Generate a count of each pixel value.
   *
   * @return List tuples [(pixel value, number of pixels with that value)] sorted by pixel value.
   */
  lazy val histogram: Seq[DicomImage.HistPoint] = {
    val list = pixelData.flatten.groupBy(identity).toList.map(g => new DicomImage.HistPoint(g._1, g._2.size)).sortWith((a, b) => a.value < b.value)
    list
  }

  def binnedHistogram(size: Int): Seq[DicomImage.HistPoint] = {
    val incr = (maxPixelValue - minPixelValue) / size
    def vToBin(v: Float): Int = ((v - minPixelValue) / incr).floor.toInt

    def binToV(bs: (Int, Int)): DicomImage.HistPoint = new DicomImage.HistPoint(((bs._1 * incr) + minPixelValue), bs._2)
    val list = pixelData.flatten.groupBy(v => vToBin(v)).toList.map(g => (g._1, g._2.size)).sortWith((a, b) => a._1 < b._1).map(bs => binToV(bs))
    list
  }

  /**
   * Create a copy rotated 90 degrees counterclockwise.  This is done in a loss-less way.
   */
  def rotate90: DicomImage = {
    new DicomImage(((width - 1) to 0 by -1).map(y => (0 until height).map(x => get(y, x))))
  }

  /**
   * Convert this image to an image that can be drawn as square pixels.  The resulting square pixels will
   * be of the size of the smaller dimension.  For example, if the pixels were 0.2 by 0.5, then the
   * resulting pixels would be 0.2 by 0.2 .
   *
   * If there is an extra fractional column (at the bottom) or row (at the right) then it will be removed.
   *
   * @param aspectRatio Width of pixel divided by height, or in other words x/y.  For example. if
   * a pixel's width is 0.5 mm and height is 0.8, then the aspect ratio would be 0.625.
   */
  def renderPixelsToSquare(pixelWidth: Double, pixelHeight: Double): DicomImage = {

    val aspectRatio = pixelWidth / pixelHeight

    /**
     * Make the image taller to accurately reflect the true height of the pixels.
     */
    def makeTaller: DicomImage = {
      def makeRow(y: Int) = {
        val topSourcePix = (aspectRatio * y).floor.toInt
        val bottomSourcePix = (aspectRatio * (y + 1)).floor.toInt

        // top and bottom of pixel
        val top = y * aspectRatio
        val bottom = top + aspectRatio

        val t = aspectRatio * y
        val b = aspectRatio * (y + 1)

        val separator = bottomSourcePix / aspectRatio

        def pixVal(x: Int): Float = {
          if ((topSourcePix == bottomSourcePix) || (separator == separator.floor))
            get(x, topSourcePix)
          else {
            val composite = ((separator - separator.floor) * get(x, topSourcePix)) + ((separator.ceil - separator) * get(x, bottomSourcePix))
            composite.toFloat
          }
        }

        (0 until width).map(x => pixVal(x))
      }

      val numRows = (height / aspectRatio).floor.toInt
      val pixArray = (0 until numRows).map(y => makeRow(y))
      val di = new DicomImage(pixArray)
      di
    }

    /**
     * Make the image wider to accurately reflect the true width of the pixels.
     */
    def makeWider: DicomImage = {
      rotate90.renderPixelsToSquare(pixelHeight, pixelWidth).rotate90.rotate90.rotate90
    }

    aspectRatio match {
      case 1.0 => this // essentially a no-op.
      case _ if (aspectRatio <= 0) => throw new InvalidParameterException("Aspect ratio must be greater than zero.  Ratio given: " + aspectRatio)
      case _ if (aspectRatio < 1) => makeTaller
      case _ => makeWider
    }
  }

  /**
   * Get the coordinates of the maximum point in the image.  The standard deviation of the point must be at least
   * larger than the given standard deviation.  The reason for this is to reject instances where the image has no
   * clear maximum.
   */
  def getMaxPoint(minStdDev: Double): Option[Point2D.Double] = {
    /**
     * Find the max point and verify that it is sufficiently large (as
     * opposed to just background noise).
     */
    def locateAndValidate(profile: Seq[Float]): Option[Double] = {
      val maxX = LocateMax.locateMax(profile)
      val spline = LocateMax.toCubicSpline(profile)
      val maxY = spline.evaluate(maxX)

      val mean = profile.sum / profile.size
      val sd = ImageUtil.stdDev(profile)
      val stdDevOfMax = (maxY - mean).abs / sd
      if (stdDevOfMax < minStdDev)
        None
      else
        Some(maxX)
    }

    val posn = Seq(locateAndValidate(columnSums), locateAndValidate(rowSums))

    val flat = posn.flatten
    if (flat.size == 2)
      Some(new Point2D.Double(flat(0), flat(1)))
    else
      None
  }
  /**
   * Get the coordinates of the upper left corner of the group of pixels of the given size that have the sum of the largest values.
   */
  def getMaxRect(xSize: Int, ySize: Int): Point2i = {
    val rectList = for (x <- 0 to (width - xSize); y <- 0 to (height - ySize)) yield { (x, y, getSubimage(new Rectangle(x, y, xSize, ySize)).sum) }
    val max = rectList.maxBy(_._3)
    new Point2i(max._1, max._2)
  }

  /**
   * Get the average pixel value within the bounds of the given rectangle.  If a
   * bound cuts through a pixel, then consider the partial weight of that pixel.
   *
   * Approach is to cut the area into 9 rectangles, center, 4 edges, and 4 corners.
   * Find the sum of each and use the grand sum to calculate the average.
   *
   * @param tblr_pix: Bounds of rectangle in pixel coordinates.
   */
  def averageOfRectangle(rect: Rectangle2D.Double): Double = {
    import java.awt.Rectangle

    // TODO this case could be handled, but there has not been a practical case that requires it.
    if ((rect.getX.floor == (rect.getX + rect.getWidth).floor) ||
      (rect.getX.floor == (rect.getX + rect.getWidth).floor))
      throw new RuntimeException("Rectangle must specify area that spans more than one pixel both vertically and horizontally.  Rectangle given: " + rect)

    val r = rect.getX + rect.getWidth // right-hand edge
    val b = rect.getY + rect.getHeight // bottom edge

    // bounds of center pixels (which comprise the majority of them)
    val x = rect.getX.ceil.toInt
    val y = rect.getY.ceil.toInt
    val w = r.floor.toInt - x
    val h = b.floor.toInt - y

    val topFrac = (rect.getY.ceil - rect.getY).toFloat
    val botFrac = (b - b.floor).toFloat
    val lftFrac = (rect.getX.ceil - rect.getX).toFloat
    val rgtFrac = (r - r.floor).toFloat

    // Sum of large area in the middle
    val centerSum = getSubimage(new Rectangle(x, y, w, h)).sum

    val topSum = getSubimage(new Rectangle(x, rect.getY.floor.toInt, w, 1)).sum * topFrac
    val botSum = getSubimage(new Rectangle(x, b.floor.toInt, w, 1)).sum * botFrac
    val lftSum = getSubimage(new Rectangle(rect.getX.floor.toInt, y, 1, h)).sum * lftFrac
    val rgtSum = getSubimage(new Rectangle(r.floor.toInt, y, 1, h)).sum * rgtFrac

    val topLft = get(rect.getX.floor.toInt, rect.getY.floor.toInt) * topFrac * lftFrac
    val topRgt = get(r.floor.toInt, rect.getY.floor.toInt) * topFrac * rgtFrac
    val botLft = get(rect.getX.floor.toInt, b.floor.toInt) * botFrac * lftFrac
    val botRgt = get(r.floor.toInt, b.floor.toInt) * botFrac * rgtFrac

    val sum = centerSum + topSum + botSum + lftSum + rgtSum + topLft + topRgt + botLft + botRgt

    val avg = sum / (rect.getWidth * rect.getHeight)
    avg
  }

  /**
   * Return the pixels as rows and columns of numbers.  This is mostly a convenience function for debugging.
   */
  def pixelsToText: String = {
    /** Maximum width needed to display any number. */
    val numLen = Seq[Float](minPixelValue, maxPixelValue, width, height).map(n => n.toInt.toString.size).max
    def fmtTxt(t: String) = t.formatted("%" + (numLen + 1) + "s")
    def fmt(f: Float) = f.toInt.formatted("%" + (numLen + 1) + "d")

    val header = fmtTxt("") + "   " + (0 until width).map(x => fmt(x)).mkString + "\n"
    def makeRow(y: Int) = fmt(y) + " : " + pixelData(y).map(p => fmt(p)).mkString + "\n"

    val text = header + (0 until height).map(y => makeRow(y)).mkString

    text
  }

  /**
   * Create a new pixel array with the pixels processed with the given transform.
   */
  def transform(trans: Float => Float): IndexedSeq[IndexedSeq[Float]] = {
    pixelData.map(row => row.map(pixel => trans(pixel)))
  }

}

object DicomImage {

  case class PixelRating(rating: Float, x: Int, y: Int) { // extends Point(point.x, point.y) {
    override def toString = "rating: " + rating + " : " + x + ", " + y
    val isInvalid = rating.toString.toLowerCase.contains("n")
  }

  def sortPixelRating1(a: PixelRating, b: PixelRating) = a.rating.toDouble > b.rating.toDouble
  def sortPixelRating(list: Seq[PixelRating]): Seq[PixelRating] = {
    val sorted = list.sortBy(_.rating).reverse
    sorted
  }

  case class HistPoint(value: Float, count: Int);

  //  case class BadPixelSet(list: Seq[PixelRating], minBound: Float, maxBound: Float);

  def getPixelData(attributeList: AttributeList): IndexedSeq[IndexedSeq[Float]] = {
    val pixDat = attributeList.getPixelData.getShortValues
    val Rows = attributeList.get(TagFromName.Rows).getIntegerValues()(0)
    val Columns = attributeList.get(TagFromName.Columns).getIntegerValues()(0)

    val pixelDataFloat = (0 until Rows).map(c => pixDat.drop(c * Columns).take(Columns).toIndexedSeq.map(s => (s & 0xffff).toFloat))
    pixelDataFloat
  }
}
