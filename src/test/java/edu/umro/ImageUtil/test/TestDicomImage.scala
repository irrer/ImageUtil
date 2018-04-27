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
        val pngName = "target\\badPixel" + bad.x + "-" + bad.y + ".png"
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

      badList.map(w => ImageUtil.annotatePixel(badPixelImage, w.x, w.y, Color.YELLOW, w.x + ", " + w.y))
      pngFile.delete
      ImageIO.write(badPixelImage, "png", pngFile)
    }

    if (true) {
      val pngName = "target\\correctedPixels.png"
      val pngFile = new File(pngName)

      val corrected = dicomImage.correctBadPixels(badList)
      val correctedImage = corrected.toBufferedImage(Color.GREEN)
      badList.map(w => ImageUtil.annotatePixel(correctedImage, w.x, w.y, Color.YELLOW, w.x + ", " + w.y))
      pngFile.delete
      ImageIO.write(correctedImage, "png", pngFile)
    }

    println("TestDicomImage done")

  }

}