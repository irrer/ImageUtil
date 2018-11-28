package edu.umro.ImageUtil

import java.awt.geom.Rectangle2D

/**
 * Precisely locate a horizontal or vertical ridge.
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
   * Hook for testing.
   */
  def testPixelSizeList(lo: Double, hi: Double) = pixelSizeList(lo, hi)

  private def centerOfMassHorz(row: Pix, colList: Seq[Pix], width: Double, array: IndexedSeq[IndexedSeq[Float]]): Double = {
    val totalMassSum = colList.map(col => row.size * array(row.index)(col.index)).sum
    val totalMassAvg = totalMassSum / width

    // only consider values that are above the mean.  This filters out the background and diminishes the
    // influence of the original bounding rectangle.
    val relevantColList = colList.filter(col => array(row.index)(col.index) > totalMassAvg)

    // sum of relevant pixels
    val relevantMassSum = relevantColList.map(col => row.size * array(row.index)(col.index)).sum

    // position*mass sum of relevant pixels
    val relevantCenterMassSum = relevantColList.map(col => col.center * row.size * array(row.index)(col.index)).sum

    // center of mass of the relevant pixels
    val center = relevantCenterMassSum / relevantMassSum
    center
  }

  /**
   * Locate the vertical center of a ridge enclosed in the given rectangle.  The algorithm is for each row of
   * pixels, consider only pixels that are above average value for that row, and then find their center of mass,
   * which is considered to be the center of that row.  Finally, take the average of the centers of the rows and
   * return that as the center of the ridge.
   */
  def locateVertical(array: IndexedSeq[IndexedSeq[Float]], rectangle: Rectangle2D.Double): Double = {
    val rowList = pixelSizeList(rectangle.y, rectangle.y + rectangle.height)
    val colList = pixelSizeList(rectangle.x, rectangle.x + rectangle.width)

    val yList = rowList.map(row => centerOfMassHorz(row, colList, rectangle.width, array) * row.size)
    val x = yList.sum / rectangle.height
    x
  }
}