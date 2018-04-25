package edu.umro.ImageUtil.test;

import com.pixelmed.dicom.AttributeList
import io.scif.img.SCIFIOImgPlus
import ij.gui.NewImage
import ij.process.ShortProcessor
import ij.ImageStack
import com.pixelmed.dicom.TagFromName
import io.scif.img.ImgOpener
import java.awt.image.BufferedImage
import net.imglib2.view.Views
import java.awt.Color
import net.imglib2.`type`.numeric.RealType
import scala.collection.JavaConverters._
import java.io.File
import javax.imageio.ImageIO
import scala.Left
import scala.Right
import edu.umro.ImgUtilities.ImgUtilities
import edu.umro.ImageUtil.DicomImage

object TestDicomImage {

  def main(args: Array[String]): Unit = {

    val fileName = "src\\test\\resources\\TestFindWorstPixels.dcm"

    val al = new AttributeList
    al.read(new File(fileName))

    val dicomImage = new DicomImage(al)

    val bufImage = dicomImage.toBufferedImage(Color.CYAN)

    val pngName = "target\\foo.png"
    val pngFile = new File(pngName)
    pngFile.delete
    ImageIO.write(bufImage, "png", pngFile)

    val worst = dicomImage.findWorstPixels(5)

    println("worst:\n    " + worst.mkString("\n    "))

    //    val sampleSize = 150
    //    val maxBadPixels = 10
    //    val stdDevMultiple = 10.0

    val badList = dicomImage.identifyBadPixels(150, 10, 10.0)

    println("Bad pixels: " + badList.mkString("\n    ", "\n    ", "\n    "))

    //    println("maxBadPixels: " + maxBadPixels)
    //    println("mean: " + mean)
    //    println("variance: " + variance)
    //    println("stdDev: " + stdDev)
    //    def fmt(d: Double) = d.formatted("%6.1f")
    //    worst.map(w => println("    diff: " + fmt(Math.abs(w.rating - mean)) + "    " + fmt(w.rating) + "  " + w.x + "," + w.y))
    //    println

    println("TestDicomImage done")

  }

}