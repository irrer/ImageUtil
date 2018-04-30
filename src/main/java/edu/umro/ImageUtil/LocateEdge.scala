package edu.umro.ImageUtil

import java.security.InvalidParameterException
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
   * Locate an edge to a precise degree.  The assumptions made are that there are both a low and high plateau that can be
   * used as lower and upper value bounds, and that there is a single, relatively smooth edge.
   */
  def locateEdge(profile: IndexedSeq[Float], plateauSampleSize: Int): Double = {
    if ((plateauSampleSize * 2) >= profile.size)
      throw new InvalidParameterException("profile should be more than twice the size of the plateauSampleSize.  profile size: " + profile.size + "    plateauSampleSize: " + plateauSampleSize)

    val sorted = profile.sorted
    val lower = sorted.take(plateauSampleSize).sum / plateauSampleSize
    val upper = sorted.takeRight(plateauSampleSize).sum / plateauSampleSize
    val halfwayVal = (upper + lower) / 2

    // index of profile that has the largest value less then halfway
    val nearEdge = profile.indices.filter(i => profile(i) < halfwayVal).maxBy(i => profile(i))

    val spline = toCubicSpline(profile)

    val maxSearchIteration = 64 // accurate to approximately this many bits of precision

    def binarySearch(lo: Double, hi: Double, iteration: Int): Double = {
      val loVal = spline.evaluate(lo)
      val hiVal = spline.evaluate(hi)
      val mid = (hi + lo) / 2
      val midVal = spline.evaluate(mid)

      println("mid: " + mid + "    midVal: " + midVal) // TODO rm

      0 match {
        case _ if (iteration > maxSearchIteration) => mid
        case _ if (halfwayVal < midVal) => binarySearch(lo, mid, iteration + 1)
        case _ if (halfwayVal > midVal) => binarySearch(mid, hi, iteration + 1)
        case _ if (halfwayVal == midVal) => mid // wildly improbable, but it is the perfect answer and improves testability
      }
    }

    binarySearch(nearEdge - 1.0, nearEdge + 1, 0)
  }

}