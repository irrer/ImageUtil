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
import java.awt.image.BufferedImage
import edu.umro.ImageUtil.ImageUtil
import javax.swing.JFrame
import java.awt.FlowLayout
import javax.swing.JLabel
import javax.swing.ImageIcon
import edu.umro.ImageUtil.ImageText
import java.awt.RenderingHints

class TestImageUtil_profileMaxCubic extends FlatSpec with Matchers {

  "TestImageUtil_profileMaxCubic" should "find maximum" in {

    // known maximum by calculation
    val max = 4.0

    // standard parabolic formula with some arbitrary parameters
    def parabola(x: Double): Double = (-0.5 * (x - max) * (x - max)) + 3

    val xList = (-5 to 10).toArray.map(x => x.toDouble)
    val yList = xList.map(x => parabola(x))

    val calculatedMax = ImageUtil.profileMaxCubic(xList, yList)
    val delta = (max - calculatedMax).abs / max
    val percentError = delta * 100
    println("expected max: " + max + "    calculatedMax: " + calculatedMax + "    percent error: " + percentError)

    (delta < 0.0001) should be(true)
  }

}
