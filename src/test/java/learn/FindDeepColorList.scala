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

package learn

import java.io.File
import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import java.awt.image.BufferedImage
import javax.swing.JFrame
import java.awt.FlowLayout
import javax.swing.ImageIcon
import javax.swing.JLabel
import java.awt.image.RenderedImage
import com.sun.xml.internal.messaging.saaj.util.ByteOutputStream
import javax.imageio.ImageIO
import java.io.FileOutputStream
import java.awt.Color

object FindDeepColorList {

  def rgb2Color(rgb: Int): Seq[Int] = {
    Seq(
      (rgb & 0xffffff) >> 16,
      (rgb & 0xffff) >> 8,
      (rgb & 0xff))
  }

  def rgb2Text(rgb: Int): String = {
    rgb2Color(rgb).map(c => c.formatted("%4d")).mkString("  ")
  }

  def findDeepColor: List[Int] = {

    case class RGBPix(rgb: Int, pix: Int) {
      override def toString = {
        pix.formatted("%6d") + " : " + rgb2Text(rgb)
      }
    }

    val lo = 10000
    val hi = 50000

    val minimum = lo.toFloat
    val maximum = hi.toFloat

    val range = maximum - minimum
    println("range: " + range)

    def calcColor(p: Int): Int = {

      val scaledPixel = (p - minimum) / range

      val a = (1 - scaledPixel) / 0.2
      val X = Math.floor(a).toInt
      val Y = Math.floor(255 * (a - X)).toInt

      val rgb = X match {
        case 0 => (255, Y, 0)
        case 1 => (255 - Y, 255, 0)
        case 2 => (0, 255, Y)
        case 3 => (0, 255 - Y, 255)
        case 4 => (Y, 0, 255)
        case 5 => (255, 0, 255)
        case _ => (255, 0, 255) // pixel outside range
      }
      val color = (rgb._1 << 8) + (rgb._2 << 16) + rgb._3
      color
    }

    val colorList = (lo until hi).map(p => (calcColor(p), p)).toMap.toList.sortWith((a, b) => (a._2 < b._2)).map(cp => cp._1)
    println("colorList.size: " + colorList.size)
    if (true) {
      println("red size: " + colorList.map(c => (c >> 16) & 0xff).distinct.size)
      println("grn size: " + colorList.map(c => (c >> 8) & 0xff).distinct.size)
      println("blu size: " + colorList.map(c => (c >> 0) & 0xff).distinct.size)
    }
    colorList
  }

  private def alToCorrected(al: AttributeList): DicomImage = {
    val rawImage = new DicomImage(al)
    val image = rawImage.correctBadPixels(rawImage.identifyBadPixels(400, 2.0, 10.0, 5, 10.0), 5)
    image
  }

  def main(args: Array[String]): Unit = {

    val start = System.currentTimeMillis

    //    val colorList = findDeepColor.map(rp => (rp.rgb, rp.pix)).toMap.toList.map(rp => new RGBPix(rp._1, rp._2)).sortWith((a, b) => a.pix < b.pix)
    //
    //    println("Size of colorList: " + colorList.size)
    //    println(colorList.toList.mkString("\n  ", "\n  ", "\n  "))
    //    println("Size of colorList: " + colorList.size)

    val elapsed = System.currentTimeMillis - start
    println("Done.  Elapsed ms: " + elapsed)
  }

}