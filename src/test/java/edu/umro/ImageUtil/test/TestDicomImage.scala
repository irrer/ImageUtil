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

object TestDicomImage {

  def main(args: Array[String]): Unit = {

    val fileName = "src\\test\\resources\\TestFindWorstPixels.dcm"

    val al = new AttributeList
    al.read(new File(fileName))

    val dicomImage = new DicomImage(al)
    val radius = 8
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

      val badPixQuickly = sub.testFindWorstPixelsQuickly(maxBad, radius)
      println("badPixQuickly:\n    " + badPixQuickly.mkString("\n    "))

      val badPix = sub.identifyBadPixels(maxBad, 1.0, radius)
      println("badPix:\n" + badPix.mkString("\n    "))
      //System.exit(0)
    }

    if (false) {

      val minExpected = IndexedSeq(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 20, 41, 56, 56, 65, 77, 89, 91, 95, 96, 99, 108,
        119, 120, 126, 129, 131, 133, 143, 144, 147, 148, 152, 154, 155, 164, 164, 167)

      val min = dicomImage.minPixelValues(minExpected.size).map(p => Math.round(p).toInt)
      println("minPixelValues: " + dicomImage.minPixelValues(40).map(p => Math.round(p).toInt).mkString("  "))
      println("minPixelValues test: " + (if (min == minExpected) "passed" else "failed"))

      val maxExpected = IndexedSeq(35208, 35208, 35209, 35212, 35214, 35215, 35215, 35216, 35216, 35223, 35227, 35228, 35229, 35230,
        35233, 35234, 35236, 35239, 35240, 35244, 35252, 35253, 35256, 35258, 35267, 35270, 35275, 35276, 35289, 35291,
        35293, 35298, 35302, 35307, 35352, 35384, 35384, 35392, 35427, 35488)

      val max = dicomImage.maxPixelValues(maxExpected.size).map(p => Math.round(p).toInt)
      println("maxPixelValues: " + max.mkString("  "))
      println("maxPixelValues test: " + (if (max == maxExpected) "passed" else "failed"))
    }

    if (false) {
      val bufImage = dicomImage.toBufferedImage(Color.CYAN)
      val pngName = "target\\original.png"
      val pngFile = new File(pngName)
      pngFile.delete
      ImageIO.write(bufImage, "png", pngFile)
    }

    //    val maxBadPixels = 10
    //    val stdDevMultiple = 3.0

    Trace.trace;
    val badList = dicomImage.identifyBadPixels(maxBadPixels, stdDev, radius)
    Trace.trace;
    val corrected = dicomImage.correctBadPixels(badList, radius)
    Trace.trace;
    println("old 746, 1: " + dicomImage.get(746, 1))
    println("new 746, 1: " + corrected.get(746, 1))
    println("dicomImage   min: " + dicomImage.min + "    max: " + dicomImage.max)
    println("correctedDicomImage   min: " + corrected.min + "    max: " + corrected.max)

    println("Bad pixels size: " + badList.size + "\n" + badList.map(b => b.toString + "    value: " + dicomImage.get(b.point.getX.toInt, b.point.getY.toInt)).mkString("\n    ", "\n    ", "\n    "))

    Trace.trace;
    val badPixelImage = dicomImage.toDeepColorBufferedImage(corrected.min, corrected.max)
    Trace.trace;

    val outDir = new File("target\\images")
    outDir.mkdirs
    outDir.listFiles.map(f => f.delete)

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

      println("dicomImage min: " + dicomImage.min + "      dicomImage max: " + dicomImage.max)
      println("corrected  min: " + corrected.min + "      corrected  max: " + corrected.max)
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