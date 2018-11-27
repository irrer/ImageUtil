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
import edu.umro.ImageUtil.BicubicRidge
import java.awt.geom.Rectangle2D
import java.awt.Color

class TestBicubicRidge extends FlatSpec with Matchers {
  val fileName = "src\\test\\resources\\TestFindWorstPixels.dcm"
  val al = new AttributeList
  al.read(fileName)
  val image = new DicomImage(al)
  val width = 18
  val height = 22
  val zoom = 10
  val center = image.getSubArray(new Rectangle(400 + 52, 400 + 40, width, height)).map(row => row.map(col => col.toDouble).toArray).toArray
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

  val outDir = new File("""target\bicubicTest""")
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
    Trace.trace("Creating original image: " + imageFile.getAbsolutePath)
    ImageIO.write(origImage, "png", imageFile)
  }

  def verticalCOM(rect: Rectangle2D.Double): Double = {
    val bottom = rect.y + rect.height
    val topPixelY = rect.y.floor.toInt
    val bottomPixelY = bottom.floor.toInt // last pixel inclusive.  Use 'to' instead of 'until'.
    val topWeight = rect.y - rect.y.floor
    val bottomWeight = bottom - bottom.floor

    val leftPixelX = rect.x.floor.toInt
    val rightPixelX = (rect.x + rect.width).floor.toInt

    /**
     * Get center of mass for given row of Y pixels within the bounds of <code>rect</code>.
     */
    def comOf(y: Int): Double = {

      ???
    }

    lazy val firstPixelValue = comOf(topPixelY) * topWeight
    lazy val lastPixelValue = comOf(bottomPixelY) * bottomWeight

    val sum: Double = 0 match {
      case _ if (topPixelY == bottomPixelY) => firstPixelValue
      case _ => firstPixelValue + (topPixelY + 1 until bottomPixelY).map(y => comOf(y)).sum + lastPixelValue
    }

    val mid = sum / rect.height
    mid
  }

  /**
   * Make png of bicubic
   */
  def bicubicPng(center: Array[Array[Double]]) = {
    def int2Array(max: Int) = (0 until max).map(x => x.toDouble).toArray
    val bc = new BicubicInterpolation(int2Array(height), int2Array(width), center)

    val bicubicImage = new BufferedImage(width * zoom, height * zoom, BufferedImage.TYPE_INT_RGB)

    val dz = zoom.toDouble
    Trace.trace
    for (x <- 0 until width * zoom) {
      for (y <- 0 until height * zoom) {
        val v = bc.interpolate(y / dz, x / dz)
        bicubicImage.setRGB(x, y, v2grey(v))
      }
    }
    val indSeq = center.map(row => row.toIndexedSeq.map(f => f.toFloat)).toIndexedSeq
    val br = new BicubicRidge(indSeq)
    //val mid = br.vertical(new Rectangle2D.Double(0, 0, width, height), 0.01)
    val mid = verticalCOM(new Rectangle2D.Double(0, 0, width, height))
    Trace.trace("bicubic mid: " + mid)

    if (true) {
      val cntrList = indSeq.map(row => ImageUtil.centerOfMass(row))
      val massMid = cntrList.sum / cntrList.size
      Trace.trace("mass mid: " + massMid)
    }

    val graphics = ImageUtil.getGraphics(bicubicImage)
    graphics.setColor(Color.orange)
    val midInt = (mid * zoom).round.toInt
    graphics.drawLine(midInt, 0, midInt, height * zoom)

    val imageFile = new File(outDir, "bicubic.png")
    Trace.trace("Creating bicubic image: " + imageFile.getAbsolutePath)

    ImageIO.write(bicubicImage, "png", imageFile)

    /**
     * Examine what happens when an point is requested that is out of the original bounds.  The interpolate method still returns a value.
     */
    if (false) {

      for (i <- 0 to 40) {
        val f = i / 8.0
        val x = 0 - 1 + f
        Trace.trace("trying " + x)
        val v = bc.interpolate(0, x)
        Trace.trace("did    " + x + "    v: " + v)
      }

      for (i <- 0 to 40) {
        val f = i / 8.0
        val x = width - 1 + f
        Trace.trace("trying " + x)
        val v = bc.interpolate(0, x)
        Trace.trace("did    " + x + "    v: " + v)
      }

    }
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
