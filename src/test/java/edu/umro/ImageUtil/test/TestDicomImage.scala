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

    println("TestDicomImage done")

  }

}