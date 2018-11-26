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

//import scala.collection.JavaConverters._

object TestDicomImageFullImage {

  def main(args: Array[String]): Unit = {

    val fileName = "src\\test\\resources\\TestFindWorstPixels.dcm"

    val al = new AttributeList
    al.read(new File(fileName))

    val dicomImage = new DicomImage(al)
    val radius = 5
    val BadPixelMaximumPercentChange = 10.0
    val BadPixelMinimumDeviation_CU = 10.0
    val maxBadPixels = 100
    val stdDev = 1.0

    Trace.trace;
    val beforeIdentify = System.currentTimeMillis
    val badList = dicomImage.identifyBadPixels(maxBadPixels, stdDev, BadPixelMaximumPercentChange, radius, BadPixelMinimumDeviation_CU)
    val afterIdentify = System.currentTimeMillis

    Trace.trace("Time in ms to identify bad pixels: " + (afterIdentify - beforeIdentify));

    val beforeCorrected = System.currentTimeMillis
    val corrected = dicomImage.correctBadPixels(badList, radius)
    val afterCorrected = System.currentTimeMillis
    Trace.trace("Time in ms to correct bad pixels: " + (afterCorrected - beforeCorrected));

    Trace.trace;
    println("old 746, 1: " + dicomImage.get(746, 1))
    println("new 746, 1: " + corrected.get(746, 1))
    println("dicomImage   min: " + dicomImage.minPixelValue + "    max: " + dicomImage.maxPixelValue)
    println("correctedDicomImage   min: " + corrected.minPixelValue + "    max: " + corrected.maxPixelValue)

    println("Bad pixels size: " + badList.size + "\n" + badList.map(b => b.toString + "    value: " + dicomImage.get(b.x, b.y)).mkString("\n    ", "\n    ", "\n    "))

    Trace.trace
    if (true) {
      val allPix = for (x <- 0 until corrected.width; y <- 0 until corrected.height) yield (DicomImage.PixelRating(corrected.get(x, y), x, y))
      val bigPix = allPix.sortBy(_.rating).takeRight(20).reverse
      println("bigPix:\n    " + bigPix.mkString("\n    bigPix "))
    }

    Trace.trace("corrected.minPixelValue: " + corrected.minPixelValue + "    corrected.maxPixelValue: " + corrected.maxPixelValue)
    val badPixelImage = dicomImage.toDeepColorBufferedImage(corrected.minPixelValue, corrected.maxPixelValue)
    Trace.trace;

    val outDir = new File("target\\images")
    outDir.mkdirs
    def deleteOldFiles: Unit = {
      val list = outDir.listFiles.filter(f => f.isFile)
      Trace.trace("Number of old files to delete: " + list.size)
      if (list.nonEmpty) {
        list.map(f => f.delete)
        Thread.sleep(1000)
        deleteOldFiles
      }
    }
    deleteOldFiles

    if (true) {
      val hw = radius * 2 + 1
      def makeBadPixelImage(bad: DicomImage.PixelRating) = {
        try {
          val pngName = "badPixel" + bad.x + "-" + bad.y + ".png"
          val pngFile = new File(outDir, pngName)
          val bufImage = ImageUtil.magnify(badPixelImage.getSubimage(Math.max(bad.x - radius, 0), Math.max(bad.y - radius, 0), hw, hw), 16)
          pngFile.delete
          ImageIO.write(bufImage, "png", pngFile)
        } catch {
          case t: Throwable => println("Could not make magnified bad pixel image for pixel " + bad.x + ", " + bad.y + " with radius " + radius)
        }
      }

      badList.map(w => makeBadPixelImage(w))
    }

    if (true) {
      val hist = dicomImage.histogram
      println("dicomImage lo hist: " + hist.take(10).mkString("    "))
      println("dicomImage hi hist: " + hist.takeRight(10).mkString("    "))
    }

    if (true) {
      val pngName = "badPixels.png"
      val pngFile = new File(outDir, pngName)

      println("Writing file: " + pngFile.getAbsolutePath)
      badList.map(w => ImageUtil.annotatePixel(badPixelImage, w.x, w.y, w.x + ", " + w.y, true))
      pngFile.delete
      ImageIO.write(badPixelImage, "png", pngFile)
    }

    if (true) {
      val pngName = "FcorrectedPixels.png"
      val pngFile = new File(outDir, pngName)

      println("dicomImage min: " + dicomImage.minPixelValue + "      dicomImage max: " + dicomImage.maxPixelValue)
      println("corrected  min: " + corrected.minPixelValue + "      corrected  max: " + corrected.maxPixelValue)
      Trace.trace;
      val correctedImage = corrected.toDeepColorBufferedImage
      Trace.trace;
      badList.map(w => ImageUtil.annotatePixel(correctedImage, w.x, w.y, w.x + ", " + w.y, true))
      pngFile.delete
      println("Writing file: " + pngFile.getAbsolutePath)
      ImageIO.write(correctedImage, "png", pngFile)

      val corHist = corrected.histogram
      println("corrected lo corHist: " + corHist.take(10).mkString("    "))
      println("corrected hi corHist: " + corHist.takeRight(10).mkString("    "))

    }

    println("TestDicomImage done")

  }

}