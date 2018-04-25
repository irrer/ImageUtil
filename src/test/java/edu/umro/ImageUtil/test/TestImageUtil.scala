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

object TestImageUtil {

  def main(args: Array[String]): Unit = {

    val fileName = "src\\test\\resources\\TestFindWorstPixels.dcm"

    ImgUtilities
    val imgPlus = ImgUtilities.readDicomFile(fileName).right.get
    val minMax = ImgUtilities.minMaxValuesOf(imgPlus)
    println("minMax: " + minMax)

    val bufImg = ImgUtilities.imgPlusToBufferedImage(imgPlus, Color.CYAN)
    val pngName = "target\\foo.png"
    val pngFile = new File(pngName)
    pngFile.delete
    ImageIO.write(bufImg, "png", pngFile)

    println("done")

  }

}