/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.umro.ImageUtil

import org.opensourcephysics.numerics.CubicSpline

import scala.annotation.tailrec

/**
  * Edge location utilities.
  */
case class Profile(data: Seq[Float]) {

  /**
    * Convert a list to a cubic spline
    */
  val cubicSpline = new CubicSpline(data.indices.toArray.map(s => s.toDouble), data.toArray.map(f => f.toDouble))

  val mean: Float = data.sum / data.size

  val standardDeviation: Double = ImageUtil.stdDev(data)

  /** Maximum number of iterations to approximate answer. */
  private val minIteration = 10

  /** Maximum number of iterations to approximate answer. */
  private val maxIteration = 20

  /**
    * Locate the maximum value to a precise degree.  The assumption made is that there is one point that is the
    * highest.  The algorithm might fail if given a profile with a series of peaks that are similar in height.
    */
  val max: Double = {

    case class XY(x: Double, y: Double) {
      override def toString: String = {
        x.formatted("%20.14f") + ", " + y.formatted("%20.14f")
      }
    }

    // Largest possible answer (end of array).  Never return a value larger than this.  The lower limit is always 0.
    val upperLimit = data.size - 1.0

    val divisions = 10

    @tailrec
    def approximate(xyList: IndexedSeq[XY], iteration: Int, prevX: Double): Double = {
      val best = xyList.maxBy(_.y)
      //Trace.trace("iteration: " + iteration.formatted("%3d") + "    best: " + best.x + "  " + best.y)
      if (
        (iteration >= maxIteration) ||
        ((best.x == prevX) && (iteration > minIteration))
      )
        best.x
      else {
        val prevIncr = xyList(1).x - xyList.head.x
        val lo = Math.max(best.x - prevIncr, 0.0)
        val hi = Math.min(best.x + prevIncr, upperLimit)
        val incr = (hi - lo) / divisions

        val newList = (0 to divisions).map(i => XY((i * incr) + lo, cubicSpline.evaluate((i * incr) + lo)))
        //Trace.trace("\n    " + newList.mkString("\n    "))
        approximate(newList, iteration + 1, best.x)
      }
    }

    val incr = 1.0 / divisions
    val list = (0 to ((data.size - 1) * divisions)).map(i => XY(i * incr, cubicSpline.evaluate(i * incr)))
    val result = approximate(list, 0, -1)

    result
  }

}
