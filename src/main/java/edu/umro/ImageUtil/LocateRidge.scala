package edu.umro.ImageUtil

import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage
import edu.umro.ScalaUtil.Trace

/**
 * Precisely locate a horizontal or vertical ridge within a given rectangle.  It is assumed that
 * the rectangle contains exactly one ridge.
 */
object LocateRidge {

  case class Pix(index: Int, size: Double, center: Double) {
    override def toString = "index: " + index + "  size: " + size + "  center: " + center
  }

  private def pixelSizeList(lo: Double, hi: Double): Seq[Pix] = {
    def sizeOf(index: Int) = {
      val iLo = Math.max(index, lo)
      val iHi = Math.min(index + 1, hi)
      val size = iHi - iLo

      val center: Double =
        if (iLo == index)
          index + (size / 2)
        else
          index + 1 - (size / 2)

      new Pix(index, size, center)
    }

    val firstPixel = lo.floor.toInt
    val lastPixel: Int = {
      if (hi.floor == hi) (hi - 1).toInt
      else hi.floor.toInt
    }
    val pixList = (firstPixel to lastPixel).map(i => sizeOf(i))
    pixList
  }

  /**
   * precisely locate the center of mass for a vertical ridge by finding the center of mass for each row of pixels and then taking the mean.
   */
  private def centerOfMassVert(row: Pix, colList: Seq[Pix], width: Double, array: IndexedSeq[IndexedSeq[Float]]): Double = {
    val totalMassSum = colList.map(col => col.size * array(row.index)(col.index)).sum
    val totalMassAvg = totalMassSum / width

    // only consider values that are above the mean.  This filters out the background and diminishes the
    // influence of the original bounding rectangle.
    val relevantColList = colList.filter(col => array(row.index)(col.index) > totalMassAvg)

    // sum of relevant pixels
    val relevantMassSum = relevantColList.map(col => col.size * array(row.index)(col.index)).sum

    // position*mass sum of relevant pixels
    val relevantCenterMassSum = relevantColList.map(col => col.center * col.size * array(row.index)(col.index)).sum

    // center of mass of the relevant pixels
    val center = relevantCenterMassSum / relevantMassSum
    center
  }

  /**
   * precisely locate the center of mass for a horizontal ridge by finding the center of mass for each column of pixels and then taking the mean.
   */
  private def centerOfMassHorz(col: Pix, rowList: Seq[Pix], height: Double, array: IndexedSeq[IndexedSeq[Float]]): Double = {
    //Trace.trace("col: " + col.index.formatted("%5d") + " : " + rowList.map(row => array(row.index)(col.index).toInt.formatted("%5d")).mkString)

    val center =
      if (false) {
        val totalMassSum = rowList.map(row => row.size * array(row.index)(col.index)).sum
        val totalMassAvg = totalMassSum / height
        // only consider values that are above the mean.  This filters out the background and diminishes the
        // influence of the original bounding rectangle.
        val relevantRowList = rowList.filter(row => array(row.index)(col.index) > totalMassAvg)

        // sum of relevant pixels
        val relevantMassSum = relevantRowList.map(row => row.size * array(row.index)(col.index)).sum

        //Trace.trace("rowList.size: " + rowList.size + "    relevantRowList.size: " + relevantRowList.size)
        // position*mass sum of relevant pixels
        val relevantCenterMassSum = relevantRowList.map(row => row.center * row.size * array(row.index)(col.index)).sum

        // center of mass of the relevant pixels
        relevantCenterMassSum / relevantMassSum
      } else {
        val totalMassSum = rowList.map(row => row.size * array(col.index)(row.index)).sum
        val relevantCenterMassSum = rowList.map(row => row.center * row.size * array(col.index)(row.index)).sum
        relevantCenterMassSum / totalMassSum
      }
    center
  }

  /**
   * Locate the center of a vertical ridge enclosed in the given rectangle.  The algorithm is for each row of
   * pixels, consider only pixels that are above average value for that row, and then find their center of mass,
   * which is considered to be the center of that row.  Finally, take the average of the centers of the rows and
   * return that as the center of the ridge.
   *
   * @param array: pixels of image
   *
   * @param rectangle: Bounding area of interest.
   */
  def locateVertical(array: IndexedSeq[IndexedSeq[Float]], rectangle: Rectangle2D.Double): Double = {
    val rowList = pixelSizeList(rectangle.y, rectangle.y + rectangle.height)
    val colList = pixelSizeList(rectangle.x, rectangle.x + rectangle.width)

    val xList = rowList.map(row => centerOfMassVert(row, colList, rectangle.width, array) * row.size)
    val x = xList.sum / rectangle.height
    x
  }

  /**
   * Locate the center of a horizontal ridge enclosed in the given rectangle.  The algorithm is for each row of
   * pixels, consider only pixels that are above average value for that row, and then find their center of mass,
   * which is considered to be the center of that row.  Finally, take the average of the centers of the rows and
   * return that as the center of the ridge.
   *
   * @param array: pixels of image
   *
   * @param rectangle: Bounding area of interest.
   */
  def locateHorizontal(array: IndexedSeq[IndexedSeq[Float]], rectangle: Rectangle2D.Double): Double = {
    val rowList = pixelSizeList(rectangle.y, rectangle.y + rectangle.height)
    val colList = pixelSizeList(rectangle.x, rectangle.x + rectangle.width)

    val yList = colList.map(col => centerOfMassHorz(col, rowList, rectangle.height, array) * col.size)
    val y = yList.sum / rectangle.width
    y
  }

  // ----------------------------------------------------------------------------------------------------------------------------

  /**
   * Hook for testing.
   */
  def testPixelSizeList(lo: Double, hi: Double) = pixelSizeList(lo, hi)

  /**
   * Hook for testing.
   */
  def testZapUnusedPixels(array: IndexedSeq[IndexedSeq[Float]], rectangle: Rectangle2D.Double): BufferedImage = {
    val rowList = pixelSizeList(rectangle.y, rectangle.y + rectangle.height)
    val colList = pixelSizeList(rectangle.x, rectangle.x + rectangle.width)

    val allPix = array.flatten
    val minPix = allPix.min
    val maxPix = allPix.max

    val range = maxPix - minPix

    val zoom = 20
    val width = array.head.size
    val height = array.size
    val bufImg = new BufferedImage(width * zoom, height * zoom, BufferedImage.TYPE_INT_RGB)
    def setPix(x: Int, y: Int, value: Float) = {
      for (xx <- 0 until zoom; yy <- 0 until zoom) {
        val level = (((value - minPix) / range) * 255).floor.toInt
        val rgb = (level << 16) + (level << 8)
        val xxx = x * zoom + xx
        val yyy = y * zoom + yy
        try {
          bufImg.setRGB(xxx, yyy, rgb)
        } catch {
          case t: Throwable => Trace.trace("out of bounds with x,y: " + xxx + ", " + yyy)
        }
      }
    }

    for (x <- 0 until width; y <- 0 until height) setPix(x, y, array(y)(x))

    def zapRow(row: Pix, colList: Seq[Pix], width: Double, array: IndexedSeq[IndexedSeq[Float]]): Unit = {
      val totalMassSum = colList.map(col => row.size * array(row.index)(col.index)).sum
      val totalMassAvg = totalMassSum / width
      colList.map(col => if (totalMassAvg > array(row.index)(col.index)) setPix(col.index, row.index, minPix))
    }

    rowList.map(row => zapRow(row, colList, rectangle.width, array))
    bufImg
  }

  /**
   * Hook for testing.
   */
  def testLocateVerticalBuf(array: IndexedSeq[IndexedSeq[Float]], rectangle: Rectangle2D.Double): BufferedImage = {

    val allPix = array.flatten
    val minPix = allPix.min
    val maxPix = allPix.max

    val range = maxPix - minPix

    val zoom = 20
    val width = array.head.size
    val height = array.size
    val bufImg = new BufferedImage(width * zoom, height * zoom, BufferedImage.TYPE_INT_RGB)
    def setPix(x: Int, y: Int, value: Float) = {
      for (xx <- 0 until zoom; yy <- 0 until zoom) {
        val rgb = (((value - minPix) / range) * 255).floor.toInt
        val xxx = x * zoom + xx
        val yyy = y * zoom + yy
        try {
          bufImg.setRGB(xxx, yyy, rgb)
        } catch {
          case t: Throwable => Trace.trace("out of bounds with x,y: " + xxx + ", " + yyy)
        }
      }
    }

    def drawLine(x: Double, y: Int) = {
      val rgb = 0xffff00
      val xx = (x * zoom).round.toInt
      for (yy <- 0 until zoom) {
        val yyy = y * zoom + yy
        try {
          bufImg.setRGB(xx, yyy, rgb)
        } catch {
          case t: Throwable => Trace.trace("out of bounds with x,y: " + xx + ", " + yyy)
        }
      }
    }

    val rowList = pixelSizeList(rectangle.y, rectangle.y + rectangle.height)
    val colList = pixelSizeList(rectangle.x, rectangle.x + rectangle.width)

    val yList = rowList.map(row => centerOfMassHorz(row, colList, rectangle.width, array) * row.size)

    for (x <- 0 until width; y <- 0 until height) setPix(x, y, array(y)(x))

    rowList.map(row => drawLine(yList(row.index), row.index))

    bufImg
  }

}