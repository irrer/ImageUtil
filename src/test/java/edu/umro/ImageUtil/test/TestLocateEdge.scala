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

    val data = {
      val count = 10
      val range = (0 until count)

      val lo = 10.0.toFloat
      val hi = 110.0.toFloat

      def ramp(x: Int) = (((((hi - lo)) / count) * x) + lo).toFloat // linearly map index to value
      val d = range.map(i => lo) ++ range.map(i => ramp(i)) ++ range.map(i => hi)
      d
    }

    def measure(data: IndexedSeq[Float]) = {
      println("----------------------------------------------")
      println("data values:\n    " + data.indices.map(i => i.formatted("%4d : ") + data(i).formatted("%5.1f")).mkString("\n    "))

      val lo = data.min
      val hi = data.max

      val halfway = ((hi + lo) / 2)

      println("data values:\n    " + data.indices.map(i => i.formatted("%4d : ") + data(i).formatted("%5.1f")).mkString("\n    "))
      val measured = LocateEdge.locateEdge(data, halfway)
      val expectedAnswer = data.indexOf(halfway).toDouble

      println("data.size: " + data.size)
      println("expectedAnswer: " + expectedAnswer)
      println("measured edge: " + measured)

      val success = (Math.abs(expectedAnswer - measured) / expectedAnswer) < 1.0e-8

      println("success: " + success)
      success should be(true)
    }

    measure(data)
    measure(data.reverse)

  }

  //}

}