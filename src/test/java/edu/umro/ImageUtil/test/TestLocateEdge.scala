package edu.umro.ImageUtil.test;

import edu.umro.ImageUtil.LocateEdge
import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TestLocateEdge extends FlatSpec with Matchers {

  //def main(args: Array[String]): Unit = {

  // Build a data profile that looks like this:
  //
  //                         ----------
  //                        /
  //                       /
  //                      /
  //                     /
  //                    /
  //                   /
  //                  /
  //                 /
  //                /
  //               /
  //    __________/
  //

  "measured edge" should "be close calculated edge" in {

    val expectedAnswer = 15.0

    val count = 10
    val range = (0 until count)

    val lo = 1.0.toFloat
    val hi = 9.0.toFloat

    def ramp(x: Int) = (((((hi - lo)) / count) * x) + lo).toFloat // linearly map index to value
    val data = range.map(i => lo) ++ range.map(i => ramp(i)) ++ range.map(i => hi)

    val midVal = LocateEdge.locateEdge(data, count / 4)

    println("data.size: " + data.size)
    println("midVal: " + midVal)

    val success = (Math.abs(expectedAnswer - midVal) / expectedAnswer) < 1.0e-8

    println("success: " + success)

    success should be(true)
  }

  //}

}