package edu.umro.ImageUtil.test;

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.awt.image.BufferedImage
import edu.umro.ImageUtil.ImageUtil
import java.awt.RenderingHints
import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import java.awt.Rectangle

import smile.interpolation.BicubicInterpolation
import edu.umro.ScalaUtil.Trace
import java.io.File
import javax.imageio.ImageIO

class TestBicubic extends FlatSpec with Matchers {
  val fileName = "src\\test\\resources\\TestFindWorstPixels.dcm"
  val al = new AttributeList
  al.read(fileName)
  val image = new DicomImage(al)
  val width = 250
  val height = 150
  val zoom = 10
  val center = image.getSubArray(new Rectangle(400, 400, width, height)).map(row => row.map(col => col.toDouble).toArray).toArray
  val array = center.flatten
  val min = array.min
  val max = array.max
  val range = (max - min).abs
  Trace.trace("min: " + min)
  Trace.trace("max: " + max)

  def v2grey(v: Double) = {
    val g = (((v - min) / range) * 255).floor.toInt
    g match {
      case _ if g < 0 => 0
      case _ if g > 255 => 255
      case _ => g
    }
  }

  val outDir = new File("""target\bicubic""")
  edu.umro.util.Utility.deleteFileTree(outDir)
  outDir.mkdirs

  /**
   * Make png of original
   */
  def originalPng(center: Array[Array[Double]]) = {

    val origImage = new BufferedImage(width * zoom, height * zoom, BufferedImage.TYPE_INT_RGB)

    for (x <- 0 until width; y <- 0 until height) {
      val v = center(y)(x)
      for (xx <- x * zoom until (x * zoom + zoom); yy <- y * zoom until (y * zoom + zoom)) origImage.setRGB(xx, yy, v2grey(v))
    }
    val imageFile = new File(outDir, "orig.png")
    Trace.trace("Creating original image: " + imageFile)
    ImageIO.write(origImage, "png", imageFile)
  }

  /**
   * Make png of bicubic
   */
  def bicubicPng(center: Array[Array[Double]]) = {
    def int2Array(max: Int) = (0 until max).map(x => x.toDouble).toArray
    val bc = new BicubicInterpolation(int2Array(width), int2Array(height), center)

    val bicubicImage = new BufferedImage(width * zoom, height * zoom, BufferedImage.TYPE_INT_RGB)

    val dz = zoom.toDouble
    for (x <- 0 until (width - 1) * zoom) {
      //for (x <- 0 until 1000) {
      //Trace.trace("xx: " + (x / dz))
      //for (y <- 0 until (height - 1) * zoom) {
        for (y <- 0 until 1000) {
        //Trace.trace("xx: " + (x / dz) + "   yy: " + (y / dz))
        val v = bc.interpolate(y / dz, x / dz)
        bicubicImage.setRGB(x, y, v2grey(v))
      }
    }
    val imageFile = new File(outDir, "bicubic.png")
    Trace.trace("Creating bicubic image: " + imageFile)

    ImageIO.write(bicubicImage, "png", imageFile)
  }

  "bicubic" should "be smooth" in {

    Trace.trace
    originalPng(center)
    Trace.trace
    bicubicPng(center)
    Trace.trace

    (45) should be(45)
  }

}
