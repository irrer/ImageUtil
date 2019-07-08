package edu.umro.ImageUtil

import org.opensourcephysics.numerics.CubicSpline
import edu.umro.ScalaUtil.Trace

/**
 * Edge location utilities.
 */
object LocateMax {

  /**
   * Convert a list to a cubic spline
   */
  private def toCubicSpline(data: Seq[Float]): CubicSpline = new CubicSpline(data.indices.toArray.map(s => s.toDouble), data.toArray.map(f => f.toDouble))

  /** Perform the binary search up to this many times to be accurate to approximately this many bits of precision. */
  private val maxSearchIteration = 128

  private def binarySearch(lo: Double, hi: Double, prevLo: Double, prevHi: Double, iteration: Int, spline: CubicSpline): Double = {

    // If iterating too long or there is no change, then use the result we have.
    if ((iteration >= maxSearchIteration) || ((lo == prevLo) && (hi == prevHi))) {
      (hi + lo) / 2
    } else {

      val count = 5 // evenly divide into this many intervals in the region of interest
      val interval = (hi - lo) / count
      val xList = (0 to count).map(i => lo + (i * interval))
      val yList = xList.map(x => spline.evaluate(x))

      // Contains peak if point on either side is lower.
      def containsPeak(i: Int) = {
        (yList(i - 1) <= yList(i)) && (yList(i) >= (yList(i + 1)))
      }

      val mid = (1 until count).filter(i => containsPeak(i)).head

      binarySearch(xList(mid - 1), xList(mid + 1), lo, hi, iteration + 1, spline)
    }
  }

  /**
   * Locate the maximum value to a precise degree.  The assumption made is that there is one point that is the
   * highest.  The algorithm might fail if given a profile with a series of peaks that are similar in height.
   *
   */
  def locateMax(profile: Seq[Float]): Double = {

    val max = profile.max
    val initial = profile.indexOf(max)

    val lo = Math.max(0, initial - 2)
    val hi = Math.min(profile.size - 1, initial + 2)

    val spline = toCubicSpline(profile)

    val result = binarySearch(lo, hi, lo - 1, hi + 1, 0, spline)

    result
  }

}