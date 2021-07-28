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
import com.pixelmed.dicom.TagFromName
import edu.umro.DicomDict.TagByName

import java.awt.geom.Point2D
import javax.imageio.ImageIO
import edu.umro.util.Utility
import edu.umro.ImageUtil.ImageUtil

import java.awt.Color

/**
 * Test the getMaxPoint method.
 */
object TestDicomImage_getMaxPoint {

  val inDir = new File("src\\test\\resources\\GetMaxPoint")
  val outDir = new File("""target\GetMaxPoint""")

  private def readToAl(file: File): AttributeList = {
    val al = new AttributeList
    al.read(file)
    al
  }

  def di(d: Double) = d.round.toInt

  def main(args: Array[String]): Unit = {

    val searchRadius_mm = 6.0
    val bbRadius_mm = 1.5
    val minStdDev = 1.2
    val zoomScale = 21

    /**
     * Cut a square rectangle out of the middle of the image twice the size of the searchRadius_mm
     */
    def cutRect(al: AttributeList): Rectangle = {
      val pixSpace = al.get(TagByName.ImagePlanePixelSpacing).getDoubleValues
      val x_mm = pixSpace(0)
      val y_mm = pixSpace(1)
      val rows = al.get(TagFromName.Rows).getIntegerValues.head
      val cols = al.get(TagFromName.Columns).getIntegerValues.head

      val x = ((cols / 2) - (searchRadius_mm / x_mm)).round.toInt
      val y = ((rows / 2) - (searchRadius_mm / y_mm)).round.toInt
      val w = ((searchRadius_mm * 2) / x_mm).round.toInt
      val h = ((searchRadius_mm * 2) / y_mm).round.toInt

      new Rectangle(x, y, w, h)
    }

    def showMax(file: File, image: DicomImage, max: Point2D.Double) = {
      println("Found max at: " + max)
      if (true) {
        val min = image.minPixelValue
        print("     ")
        for (x <- 0 until image.width)
          print(x.formatted("%4d"))
        println
        for (y <- 0 until image.height) {
          print(y.formatted("%4d:"))
          for (x <- 0 until image.width) {
            print((image.get(x, y) - min).round.toInt.formatted("%4d"))
          }
          println
        }
      }
      println("Found max at: " + max)

      val pngFile = new File(outDir, file.getName.replaceAll(".dcm$", ".png"))
      //val bufImg = ImageUtil.magnify(image.toDeepColorBufferedImage(0), zoomScale)
      val bufImg = ImageUtil.magnify(image.toBufferedImage(Color.blue), zoomScale)
      val graphics = ImageUtil.getGraphics(bufImg)
      graphics.setColor(Color.white)
      val x = ((max.getX + 0.5) * zoomScale).floor.toInt
      val y = ((max.getY + 0.5) * zoomScale).floor.toInt
      graphics.drawLine(0, y, bufImg.getWidth - 1, y)
      graphics.drawLine(x, 0, x, bufImg.getHeight - 1)

      ImageIO.write(bufImg, "png", pngFile)
      println("Wrote image to " + pngFile.getAbsolutePath)
      println
    }

    def oneDot(image: DicomImage): DicomImage = {
      def colorOf(x: Int, y: Int): Float = {
        //if ((x == (image.width / 3)) && (y == (image.height / 5))) 100 else 0
        if ((x == 5) && (y == 5)) 100000 else 10000
      }
      val pixels = (0 until image.width).map(y => (0 until image.height).map(x => colorOf(x, y)))
      new DicomImage(pixels)
    }

    def testImage(file: File) = {
      println("Processing file " + file.getAbsolutePath)
      val al = readToAl(file)
      val wholeImage = new DicomImage(al)

      val pixSpace = al.get(TagByName.ImagePlanePixelSpacing).getDoubleValues
      val x_mm = pixSpace(0)
      val y_mm = pixSpace(1)
      val xSizeBB = ((bbRadius_mm * 2) / x_mm).round.toInt
      val ySizeBB = ((bbRadius_mm * 2) / y_mm).round.toInt

      val centerRect = cutRect(al)
      val centerImage = wholeImage.getSubimage(centerRect)
      // val image = oneDot(cutRect(al))
      val xyMaxCorner = centerImage.getMaxRect(xSizeBB, ySizeBB)

      val bbRect = {
        val bbRectX = centerRect.getX + xyMaxCorner.getX - (xSizeBB / 2.0)
        val bbRectY = centerRect.getY + xyMaxCorner.getY - (ySizeBB / 2.0)
        val bbRectW = xSizeBB * 2.0
        val bbRectH = ySizeBB * 2.0
        new Rectangle(di(bbRectX), di(bbRectY), di(bbRectW), di(bbRectH))
      }

      val bbImage = wholeImage.getSubimage(bbRect)

      bbImage.getMaxPoint(minStdDev) match {
        case Some(bbMax) => {
          showMax(file, bbImage, bbMax)
        }
        case _ => println("Did not find a max point")
      }
    }

    //System.setProperty("log4j2.debug", "INFO")
    val alList = inDir.listFiles.map(f => readToAl(f))
    Utility.deleteFileTree(outDir)
    outDir.mkdirs

    inDir.listFiles.map(file => testImage(file))

    println("TestDicomImage_getMaxPoint done")

  }

}