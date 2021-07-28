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

import com.pixelmed.dicom.AttributeList
import java.io.File
import edu.umro.ImageUtil.DicomImage
import java.awt.Rectangle

/**
 * Analyze a small area of pixels to see whether the DicomImage.findWorstPixelsQuickly method is working.
 */
object TestDicomImageQuickly {

  def main(args: Array[String]): Unit = {

    val fileName = "src\\test\\resources\\TestFindWorstPixels.dcm"

    val al = new AttributeList
    al.read(new File(fileName))

    val dicomImage = new DicomImage(al)
    val radius = 8
    val BadPixelMaximumPercentChange = 10.0
    val BadPixelMinimumDeviation_CU = 10.0
    val maxBadPixels = 100
    val stdDev = 1.0

    if (true) {
      val sub = dicomImage.getSubimage(new Rectangle(555, 0, 25, 20))

      val maxBad = 10
      val diam = radius * 2 + 1
      println("Using a radius of " + radius + "    diam: " + diam)

      def printValues = {
        def fmt(v: Float) = v.toInt.formatted("%4d")
        println
        print("       ")
        for (x <- 0 until sub.width) { print(fmt(x) + "  ") }
        println
        for (y <- 0 until sub.height) {
          print(fmt(y) + " : ")
          println((0 until sub.width).map(x => fmt(sub.get(x, y))).mkString(" +"))
        }
        println
      }
      printValues

      val badPixQuickly = sub.testFindWorstPixelsQuickly(maxBad, radius, BadPixelMinimumDeviation_CU)
      println("badPixQuickly:\n    " + badPixQuickly.mkString("\n    "))

      val badPix = sub.identifyBadPixels(maxBad, 1.0, BadPixelMaximumPercentChange, radius, BadPixelMinimumDeviation_CU)
      println("badPix:\n" + badPix.mkString("\n    "))
      //System.exit(0)
    }

    println("TestDicomImageQuickly done")

  }

}