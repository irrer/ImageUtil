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
//import scala.collection.JavaConverters._

object TestDicomImage {

  def main(args: Array[String]): Unit = {

    val fileName = "src\\test\\resources\\TestFindWorstPixels.dcm"

    val al = new AttributeList
    al.read(new File(fileName))

    val dicomImage = new DicomImage(al)

    if (true) {

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

    if (true) {
      val bufImage = dicomImage.toBufferedImage(Color.CYAN)
      val pngName = "target\\original.png"
      val pngFile = new File(pngName)
      pngFile.delete
      ImageIO.write(bufImage, "png", pngFile)
    }

    //    val sampleSize = 150
    //    val maxBadPixels = 10
    //    val stdDevMultiple = 10.0

    val badList = dicomImage.identifyBadPixels(150, 10, 10.0)

    println("Bad pixels: " + badList.mkString("\n    ", "\n    ", "\n    "))

    val badPixelImage = dicomImage.toBufferedImage(Color.GREEN)

    if (true) {
      val radius = 3
      val hw = radius * 2 + 1
      def makeBadPixelImage(bad: DicomImage.PixelRating) = {
        val pngName = "target\\badPixel" + bad.x + ".png"
        val pngFile = new File(pngName)
        val bufImage = ImageUtil.magnify(badPixelImage.getSubimage(bad.x - radius, bad.y - radius, hw, hw), 16)
        pngFile.delete
        ImageIO.write(bufImage, "png", pngFile)
      }

      badList.map(w => makeBadPixelImage(w))
    }

    if (true) {
      val pngName = "target\\badPixels.png"
      val pngFile = new File(pngName)

      badList.map(w => ImageUtil.annotatePixel(badPixelImage, w.x, w.y, Color.YELLOW, w.x + ", " + w.y, true))
      pngFile.delete
      ImageIO.write(badPixelImage, "png", pngFile)
    }

    if (true) {
      val pngName = "target\\correctedPixels.png"
      val pngFile = new File(pngName)

      val corrected = dicomImage.correctBadPixels(badList)
      val correctedImage = corrected.toBufferedImage(Color.GREEN)
      badList.map(w => ImageUtil.annotatePixel(correctedImage, w.x, w.y, Color.YELLOW, w.x + ", " + w.y, true))
      pngFile.delete
      ImageIO.write(correctedImage, "png", pngFile)
    }

    println("TestDicomImage done")

  }

}