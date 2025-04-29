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

/**
  * Edge location utilities.
  */
object LocateEdge {

  /**
    * Convert a list to a cubic spline
    */
  private def toCubicSpline(data: IndexedSeq[Float]): CubicSpline = new CubicSpline(data.indices.toArray.map(s => s.toDouble), data.toArray.map(f => f.toDouble))

  /** Perform the binary search up to this many times to be accurate to approximately this many bits of precision. */
  private val maxSearchIteration = 128

  private def binarySearch(lo: Double, hi: Double, prevLo: Double, prevHi: Double, iteration: Int, spline: CubicSpline, edgeVal: Double): Double = {
    val mid = (hi + lo) / 2
    val midVal = spline.evaluate(mid)

    val result = 0 match {
      case _ if iteration > maxSearchIteration =>
        mid
      case _ if (lo == prevLo) && (hi == prevHi) =>
        mid
      case _ if edgeVal < midVal =>
        binarySearch(lo, mid, lo, hi, iteration + 1, spline, edgeVal)
      case _ if edgeVal > midVal =>
        binarySearch(mid, hi, lo, hi, iteration + 1, spline, edgeVal)
      case _ if edgeVal == midVal =>
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
    *
    * @param profile Profile of the edge, either going from low to high or high to low.
    * @param threshold Where the precise location of the edge should be. Usually this value is the 50% point.
    *                  For example, if the low value was 5 and the high value was 11, then this would be 8.
    *
    */
  def locateEdge(profile: IndexedSeq[Float], threshold: Double): Double = {

    val reversed = profile.head > profile.last
    val profileLoToHi = if (reversed) profile.reverse else profile

    validate(profileLoToHi, threshold)
    val lo = -1
    val hi = profileLoToHi.size

    val spline = toCubicSpline(profileLoToHi)

    val result = binarySearch(lo, hi, lo - 1, hi + 1, 0, spline, threshold)

    if (reversed)
      profileLoToHi.size - 1 - result
    else
      result

  }

}
