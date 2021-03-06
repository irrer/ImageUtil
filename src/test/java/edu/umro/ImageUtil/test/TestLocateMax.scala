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

package edu.umro.ImageUtil.test;

import edu.umro.ImageUtil.LocateEdge
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import edu.umro.ImageUtil.LocateMax

class TestLocateMax extends FlatSpec with Matchers {

  // Build a data profile that looks something like this:
  //
  //
  //
  //                    /\
  //                   /  \
  //                  /    \
  //                 /      \
  //                /        \
  //               /          \
  //    __________/            \_______________________________
  //

  "measured edge" should "be close calculated edge" in {

    val data = {

      val lo = 10.0.toFloat

      val flat = (0 to 5).map(i => lo)
      val rise = (1 to 5).map(i => (i * 2) + lo)
      val fall = rise.reverse.tail

      flat ++ rise ++ fall ++ flat ++ flat ++ flat
    }

    def measure(data: IndexedSeq[Float]) = {
      println("----------------------------------------------")
      println("data values:\n    " + data.indices.map(i => i.formatted("%4d : ") + data(i).formatted("%5.1f")).mkString("\n    "))

      val lo = data.min
      val hi = data.max

      println("data values:\n    " + data.indices.map(i => i.formatted("%4d : ") + data(i).formatted("%5.1f")).mkString("\n    "))
      val measured = LocateMax.locateMax(data)
      val expectedAnswer = 10.toDouble

      println("data.size: " + data.size)
      println("expectedAnswer: " + expectedAnswer)
      println("measured max: " + measured)

      val success = (Math.abs(expectedAnswer - measured) / expectedAnswer) < 1.0e-8

      println("success: " + success)
      success should be(true)
    }

    measure(data)
    //measure(data.reverse)

  }

  //  def main(args: Array[String]): Unit = {
  //    measure(data)
  //  }

}