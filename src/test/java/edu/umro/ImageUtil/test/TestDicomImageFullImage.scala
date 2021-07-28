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
import com.pixelmed.dicom.TagFromName
import java.awt.image.BufferedImage
import java.awt.Color
import java.io.File
import javax.imageio.ImageIO
import scala.Left
import scala.Right
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import java.awt.Rectangle
import edu.umro.ScalaUtil.Trace
import edu.umro.util.Utility

//import scala.collection.JavaConverters._

object TestDicomImageFullImage {

  private val inDir = new File("""src\test\resources\TestFindWorstPixels""")
  private val outDir = new File("""target\TestFindWorstPixels""")

  private val radius = 5
  private val BadPixelMaximumPercentChange = 10.0
  private val BadPixelMinimumDeviation_CU = 10.0
  private val maxBadPixels = 100
  private val stdDev = 10.0
  private val magnify = 16 // for display of sub-images

  private def makeBadPixelImage(bad: DicomImage.PixelRating, badPixelImage: BufferedImage, outSubDir: File) = {
    val hw = radius * 2 + 1
    try {
      val pngName = "badPixel" + bad.rating.toInt.formatted("%04d") + "_" + bad.x + "-" + bad.y + ".png"
      val pngFile = new File(outSubDir, pngName)
      val bufImage = ImageUtil.magnify(badPixelImage.getSubimage(Math.max(bad.x - radius, 0), Math.max(bad.y - radius, 0), hw, hw), magnify)
      val x = ((if (bad.x >= radius) radius else bad.x) * magnify) + ((magnify / 2) - 1)
      val y = ((if (bad.y >= radius) radius else bad.y) * magnify) + ((magnify / 2) - 1)

      for (xx <- Seq(x, x + 1); yy <- Seq(y, y + 1)) bufImage.setRGB(xx, yy, 0)
      ImageIO.write(bufImage, "png", pngFile)
    } catch {
      case t: Throwable => println("Could not make magnified bad pixel image for pixel " + bad.x + ", " + bad.y + " with radius " + radius)
    }
  }

  private def processImage(file: File) = {
    println("------------- processing file " + file.getAbsolutePath)
    val outSubDirName = file.getName.replace(".dcm", "").replace(".DCM", "")
    val outSubDir = new File(outDir, outSubDirName)
    outSubDir.mkdirs

    val al = new AttributeList
    al.read(file)

    val dicomImage = new DicomImage(al)

    val beforeIdentify = System.currentTimeMillis
    val badList = dicomImage.identifyBadPixels(maxBadPixels, stdDev, BadPixelMaximumPercentChange, radius, BadPixelMinimumDeviation_CU)
    val afterIdentify = System.currentTimeMillis

    println("Time in ms to identify bad pixels: " + (afterIdentify - beforeIdentify));

    val beforeCorrected = System.currentTimeMillis
    val corrected = dicomImage.correctBadPixels(badList, radius)
    val afterCorrected = System.currentTimeMillis
    println("Time in ms to correct bad pixels: " + (afterCorrected - beforeCorrected));

    val allPix = for (x <- 0 until corrected.width; y <- 0 until corrected.height) yield (DicomImage.PixelRating(corrected.get(x, y), x, y))
    val bigPix = allPix.sortBy(_.rating).takeRight(20).reverse
    println("bigPix:\n    " + bigPix.mkString("\n    bigPix "))
    println("Bad pixels size: " + badList.size + "\n" + badList.map(b => b.toString + "    value: " + dicomImage.get(b.x, b.y)).mkString("\n    ", "\n    ", "\n    "))

    println("corrected.minPixelValue: " + corrected.minPixelValue + "    corrected.maxPixelValue: " + corrected.maxPixelValue)

    val beforeColoring = System.currentTimeMillis
    val badPixelImage = dicomImage.toDeepColorBufferedImage(0.2)
    val afterColoring = System.currentTimeMillis
    println("Time in ms toDeepColorBufferedImage: " + (afterColoring - beforeColoring));

    badList.map(w => makeBadPixelImage(w, badPixelImage, outSubDir))

    val hist = dicomImage.histogram
    println("dicomImage lo hist: " + hist.take(10).mkString("    "))
    println("dicomImage hi hist: " + hist.takeRight(10).mkString("    "))

    ImageIO.write(badPixelImage, "png", new File(outSubDir, "original.png"))

    val correctedImage = corrected.toDeepColorBufferedImage(0.2)
    ImageIO.write(correctedImage, "png", new File(outSubDir, "corrected.png"))

    badList.map(w => ImageUtil.annotatePixel(badPixelImage, w.x, w.y, w.x + ", " + w.y, true))
    ImageIO.write(badPixelImage, "png", new File(outSubDir, "originalAnnotated.png"))

    val rangeImage = dicomImage.toDeepColorBufferedImage(corrected.minPixelValue, corrected.maxPixelValue)
    ImageIO.write(rangeImage, "png", new File(outSubDir, "correctedColorRange.png"))

    val originalImage = dicomImage.toDeepColorBufferedImage(0)
    ImageIO.write(originalImage, "png", new File(outSubDir, "noColorCorrection.png"))
  }

  def main(args: Array[String]): Unit = {
    println("Starting test")

    Utility.deleteFileTree(outDir)
    outDir.mkdirs

    inDir.listFiles.toSeq.map(f => processImage(f))

    println("TestDicomImage done")
  }

}