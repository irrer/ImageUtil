package edu.umro.ImageUtil

import org.opensourcephysics.numerics.CubicSpline
import edu.umro.ScalaUtil.Trace
import java.security.InvalidParameterException

/**
 * Edge location utilities.
 */
object LocateMax {

  /**
   * Convert a list to a cubic spline
   */
  private def toCubicSpline(data: Seq[Float]): CubicSpline = new CubicSpline(data.indices.toArray.map(s => s.toDouble), data.toArray.map(f => f.toDouble))

  /** Maximum number of iterations to approximate answer. */
  private val maxIteration = 16

  /**
   * Locate the maximum value to a precise degree.  The assumption made is that there is one point that is the
   * highest.  The algorithm might fail if given a profile with a series of peaks that are similar in height.
   *
   *  @param profile List of values describing profile that contains a maximum.
   */
  def locateMax(profile: Seq[Float]): Double = {

    case class XY(x: Double, y: Double) {
      override def toString: String = {
        x.formatted("%20.14f") + ", " + y.formatted("%20.14f")
      }
    }

    // Largest possible answer (end of array).  Never return a value larger than this.  The lower limit is always 0.
    val upperLimit = profile.size - 1.0

    val spline = toCubicSpline(profile)

    val divisions = 10

    def approximate(xyList: IndexedSeq[XY], iteration: Int, prevBestX: Double): Double = {
      val best = xyList.maxBy(_.y)
      Trace.trace("iteration: " + iteration.formatted("%3d") + "    best: " + best.x + "  " + best.y)
      if ((iteration >= maxIteration) || (best.x == prevBestX)) best.x
      else {
        val prevIncr = xyList(1).x - xyList.head.x
        val lo = Math.max(best.x - prevIncr, 0.0)
        val hi = Math.min(best.x + prevIncr, upperLimit)
        val incr = (hi - lo) / divisions

        val newList = (0 to divisions).map(i => new XY(((i * incr) + lo), spline.evaluate((i * incr) + lo)))
        //Trace.trace("\n    " + newList.mkString("\n    "))
        approximate(newList, iteration + 1, best.x)
      }
    }

    val incr = 1.0 / divisions
    val list = (0 to ((profile.size - 1) * divisions)).map(i => new XY(i * incr, spline.evaluate(i * incr)))
    val result = approximate(list, 0, -1)

    result
  }

}