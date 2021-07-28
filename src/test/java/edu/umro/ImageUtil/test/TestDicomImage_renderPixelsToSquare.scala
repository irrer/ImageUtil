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
import javax.swing.JFrame
import java.awt.FlowLayout
import javax.swing.JLabel
import javax.swing.ImageIcon
import java.awt.Color
import java.awt.Dimension
import java.awt.Point

/**
 * Check the function that corrects for aspect ratio.
 */
object TestDicomImage_renderPixelsToSquare {

  val location = new Point(20, 20)

  private val dicomImage = {
    val size = 200
    val light = 1000.toFloat
    val dark = 0.toFloat

    val lightSet = Set(2, 3, 5, 7, 11, 12, 13, 39, 42, 43, 44)

    def colorOf(x: Int, y: Int) = {

      if (((x - y).abs < 5) ||
        lightSet.contains(x) ||
        lightSet.contains(y) ||
        (new Point(x, y).distance(150, 100) < 15))
        light else dark
    }

    val pixels = (0 until size).map(y => (0 until size).map(x => colorOf(x, y)))
    new DicomImage(pixels)
  }

  private def stretchAndShow(x: Double, y: Double) = {
    val img = dicomImage.renderPixelsToSquare(x, y)
    val title = "x: " + x + "    y: " + y + "    x/y: " + (x / y)
    show(img, title)
  }

  private def show(img: DicomImage, title: String) = {

    class Runny extends Runnable {
      def run = {
        val bufImg = img.toBufferedImage(Color.green)

        val frame = new JFrame
        frame.setTitle(title)
        frame.setMinimumSize(new Dimension(300, 600))
        frame.setLocation(location)
        frame.getContentPane.setLayout(new FlowLayout)
        frame.getContentPane().add(new JLabel(new ImageIcon(bufImg)))
        frame.pack
        frame.setLocationRelativeTo(null)
        frame.setVisible(true)
        frame.setLocation(location)
        location.x = location.x + 20 // I know, I know - mutability.  But hey, this is just test code.
        location.y = location.y + 20
      }
    }

    val runny = new Runny
    val thread = new Thread(runny)
    thread.start
    Thread.sleep(250)
  }

  def main(args: Array[String]): Unit = {

    show(dicomImage, "original")
    show(dicomImage.rotate90, "rotate 90")
    show(dicomImage.rotate90.rotate90, "rotate 180")
    show(dicomImage.rotate90.rotate90.rotate90, "rotate 270")
    show(dicomImage.rotate90.rotate90.rotate90.rotate90, "rotate 360")

    stretchAndShow(2, 2)
    stretchAndShow(1, 2.5)
    stretchAndShow(0.5, 1.3)
    stretchAndShow(2.5, 1)
    stretchAndShow(1.7, 1)

    // val wider = dicomImage.renderPixelsToSquare(2, 1)
    println("TestDicomImage_renderPixelsToSquare done")

    Thread.sleep(5 * 60 * 1000)
    println("TestDicomImage_renderPixelsToSquare exiting...")
    System.exit(0)

  }

}