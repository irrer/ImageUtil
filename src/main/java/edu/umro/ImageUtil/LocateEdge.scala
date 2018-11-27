package edu.umro.ImageUtil

import org.opensourcephysics.numerics.CubicSpline

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

  /** Perform the binary search this many times to be accurate to approximately this many bits of precision. */
  private val maxSearchIteration = 64

  private def binarySearch(lo: Double, hi: Double, iteration: Int, spline: CubicSpline, edgeVal: Double): Double = {
    val loVal = spline.evaluate(lo)
    val hiVal = spline.evaluate(hi)
    val mid = (hi + lo) / 2
    val midVal = spline.evaluate(mid)

    0 match {
      case _ if (iteration > maxSearchIteration) => mid
      case _ if (edgeVal < midVal) => binarySearch(lo, mid, iteration + 1, spline, edgeVal)
      case _ if (edgeVal > midVal) => binarySearch(mid, hi, iteration + 1, spline, edgeVal)
      case _ if (edgeVal == midVal) => mid // wildly improbable, but it is the perfect answer and improves testability
    }
  }

  /**
   * Locate an edge to a precise degree.  The assumptions made are that there are both a low and high plateau that can be
   * used as lower and upper value bounds, and that there is a single, relatively smooth edge.  This function defines the
   * edge as the point closest to the threshold value.
   *
   * For edge location to work, some values in the profile must be greater than the threshold and some must be
   * greater than the threshold.  If this is not true, then a value of -1 is returned.
   */
  def locateEdge(profile: IndexedSeq[Float], threshold: Double): Double = {
    val lessThan = profile.indices.filter(i => profile(i) < threshold)
    val greaterThan = profile.indices.filter(i => profile(i) > threshold)

    if (lessThan.isEmpty || greaterThan.isEmpty)
      -1
    else {
      // index of profile that has the largest value less then halfway
      val closestToEdge = lessThan.maxBy(i => profile(i))

      val spline = toCubicSpline(profile)

      binarySearch(closestToEdge - 1.0, closestToEdge + 1, 0, spline, threshold)
    }
  }

}