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

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.pixelmed.dicom.AttributeList
import java.io.File
import edu.umro.ImageUtil.DicomImage
import java.awt.Rectangle
import com.pixelmed.dicom.TagFromName
import java.awt.geom.Point2D
import javax.imageio.ImageIO
import edu.umro.util.Utility
import edu.umro.ImageUtil.ImageUtil
import java.awt.Color
import java.awt.geom.Rectangle2D

/**
 * Test the getMaxPoint method.
 */
object TestDicomImage_averageOfRectangle {

  val xBase = 5
  val yBase = 5
  val width = 8
  val height = 20

  val tolerance = 0.000001

  def valueOf(x: Int, y: Int) = {
    if ((x >= xBase) && (x < (xBase + width)) && (y >= yBase) && (y < (yBase + height))) 1.toFloat else 0.toFloat
  }

  val image = {
    // pixels for a DicomImage that is 0 except for a rectangle of 1's.
    val pixels = {
      for (y <- (0 until (yBase + height + yBase))) yield {
        for (x <- (0 until (xBase + width + xBase))) yield { valueOf(x, y) }
      }
    }
    new DicomImage(pixels)
  }

  println("pixel map:")
  for (y <- (0 until image.height)) {
    print(y.formatted("%3d: "))
    for (x <- (0 until image.width)) print(image.get(x, y).toInt)
    println
  }
  println

  def testRect(rect: Rectangle2D.Double, expected: Double): Boolean = {
    val calculated = image.averageOfRectangle(rect)
    val err = (expected - calculated).abs / expected
    val ok = err < tolerance
    def fmt(d: Double) = d.formatted("%8.6f")
    val msg = "expected: " + fmt(expected) + "   calculated: " + fmt(calculated) + "    err: " + err.formatted("%20.16f") + "    rect: " + rect
    if (ok)
      println("Success: " + msg)
    else
      println("Failure: " + msg + " ########################################")
    if (err != 0) println("err not zero: " + err.formatted("%20.16f") + "    tolerance: " + tolerance.formatted("%20.16f"))
    ok
  }

  val testSeq: Seq[((Double, Double, Double, Double), Double)] = Seq(
    ((xBase, yBase, width, height), 1.0),

    ((xBase - .5, yBase, width, height), ((width - .5) * height) / (width * height)),
    ((xBase + .5, yBase, width, height), ((width - .5) * height) / (width * height)),

    ((xBase - .25, yBase, width, height), ((width - .25) * height) / (width * height)),
    ((xBase + .25, yBase, width, height), ((width - .25) * height) / (width * height)),

    ((xBase, yBase - .25, width, height), (width * (height - .25)) / (width * height)),
    ((xBase, yBase + .25, width, height), (width * (height - .25)) / (width * height)),

    ((xBase - .25, yBase - .25, width, height), ((width - .25) * (height - .25)) / (width * height)),

    ((xBase + .125, yBase + .125, width - .75, height - .75), 1.0),
    ((xBase + .125, yBase + .125, width - .25, height - .25), 1.0),

    ((xBase + 1, yBase + .25, width - 2.5, height - 1.5), 1.0),
    ((xBase + .27, yBase + 1.15, width - 3.5, height - 6.5), 1.0),

    // ((xBase + .25, yBase + .25, .5, .5), 1.0), // TODO this should be handled but there is no practical reason to do so.

    ((xBase - .25, yBase - .25, width + .5, height + .5), ((width) * (height)) / ((width + .5) * (height + .5))))

  def main(args: Array[String]): Unit = {
    println("Tolerance: " + tolerance.formatted("%20.16f"))

    val summary = testSeq.map(t => testRect(new Rectangle2D.Double(t._1._1, t._1._2, t._1._3, t._1._4), t._2))

    val passCount = summary.filter(r => r).size

    println("\nTotal tests: " + summary.size + "    number of tests passed: " + passCount + "    number of tests failed: " + (summary.size - passCount))
  }

}