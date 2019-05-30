package edu.umro.ImageUtil

import org.opensourcephysics.numerics.CubicSpline
import edu.umro.ScalaUtil.Trace

/**
 * Edge location utilities.
 */
object LocateEdge {

  /**
   * Convert a list to a cubic spline
   */
  private def toCubicSpline(data: IndexedSeq[Float]): CubicSpline = new CubicSpline(data.indices.toArray.map(s => s.toDouble), data.toArray.map(f => f.toDouble))

  /**
   * Get the value (height) of the edge.
   */
  private def getEdgeVal(profile: IndexedSeq[Float], plateauSampleSize: Int): Float = {
    val sorted = profile.sorted
    val lower = sorted.take(plateauSampleSize).sum / plateauSampleSize
    val upper = sorted.takeRight(plateauSampleSize).sum / plateauSampleSize
    val edgeVal = (upper + lower) / 2
    edgeVal
  }

  /** Perform the binary search up to this many times to be accurate to approximately this many bits of precision. */
  private val maxSearchIteration = 128

  private def binarySearch(lo: Double, hi: Double, prevLo: Double, prevHi: Double, iteration: Int, spline: CubicSpline, edgeVal: Double): Double = {
    val loVal = spline.evaluate(lo)
    val hiVal = spline.evaluate(hi)
    val mid = (hi + lo) / 2
    val midVal = spline.evaluate(mid)

    val result = 0 match {
      case _ if (iteration > maxSearchIteration) =>
        mid
      case _ if ((lo == prevLo) && (hi == prevHi)) =>
        mid
      case _ if (edgeVal < midVal) =>
        binarySearch(lo, mid, lo, hi, iteration + 1, spline, edgeVal)
      case _ if (edgeVal > midVal) =>
        binarySearch(mid, hi, lo, hi, iteration + 1, spline, edgeVal)
      case _ if (edgeVal == midVal) =>
        mid // wildly improbable, but it is the perfect answer and improves testability
    }
    result
  }

  /**
   * Check the parameters to make sure that they are valid.
   */
  private def validate(profile: IndexedSeq[Float], threshold: Double): Unit = {
    if (threshold < profile.head)
      throw new IllegalArgumentException("threshold " + threshold + " is below lowest value " + profile.head)

    if (threshold > profile.last)
      throw new IllegalArgumentException("threshold " + threshold + " is above highest value " + profile.last)

    if (profile.size < 2)
      throw new IllegalArgumentException("At least 2 values must be given but only " + profile.size + " were provided")
  }

  /**
   * Locate an edge to a precise degree.  The assumptions made are that there are both a low and high plateau that can be
   * used as lower and upper value bounds, and that there is a single, relatively smooth edge.  This function defines the
   * edge as the point closest to the threshold value.
   *
   * For edge location to work, some values in the profile must be greater than the threshold and some must be
   * greater than the threshold.  If this is not true, then a value of -1 is returned.
   */
  def locateEdge(unorderedrofile: IndexedSeq[Float], threshold: Double): Double = {

    val reversed = unorderedrofile.head > unorderedrofile.last
    val profile = if (reversed) unorderedrofile.reverse else unorderedrofile

    validate(profile, threshold)
    val lo = -1
    val hi = profile.size

    val spline = toCubicSpline(profile)

    val result = binarySearch(lo, hi, lo - 1, hi + 1, 0, spline, threshold)

    if (reversed)
      profile.size - 1 - result
    else
      result

  }

}