package edu.umro.ImageUtil

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

object ImageUtil {

  /**
   * Container for a pair of min and max values.
   */
  case class MinMax(min: Int, max: Int) {
    def this() = this(Int.MaxValue, Int.MinValue)

    def update(v: Int) = {
      new MinMax(Math.min(min, v), Math.max(max, v))
    }
  }

  /**
   * Find the minimum and maximum values of an image.
   */
  def minMaxValuesOf(pixelData: Array[Short]): MinMax = {
    pixelData.foldLeft(new MinMax)((mm, v) => mm.update(v & 0xffff))
  }

  /**
   * Convert an imglib2 image to an RGB BufferedImage.  Note that the brightness is based on
   * the min and max pixel values for the image.  Normally DICOM image brightness is based on
   * a series-wide basis as opposed to individual slices.
   *
   * @param imgPlus: Image to convert.
   *
   * @param color: Maximum color level for pixels.  Example: <code>Color.GREEN</code>.
   */
  def floatToBufferedImage(image: DicomImage, color: Color): BufferedImage = {
    val width = imgPlus.getImg.dimension(0).toInt
    val height = imgPlus.getImg.dimension(1).toInt
    //val minMax = minMaxValuesOf(imgPlus)
    val minMax: MinMax =null

    // calculate a lookup table once
    val rgbTable = {
      val red = color.getRed
      val green = color.getGreen
      val blue = color.getBlue

      def iToRGB(i: Int) = {

        val gg = green * (i / 256.0)

        def toColor(c: Int, i: Int, shift: Int) = {
          val lev = (c * (i / 256.0)).round.toInt match {
            case _ if (i < 0) => 0
            case _ if (i > 255) => 255
            case i => i
          }
          lev << shift
        }

        toColor(red, i, 16) + toColor(green, i, 8) + toColor(blue, i, 0)
      }

      (0 until 256).map(i => iToRGB(i))
    }

    val ratio = 256 / (minMax.max - minMax.min)

    /**
     * Given a pixel level, return the RGB color.
     */
    def levelToColor(level: Float): Int = {
      val i = (level * ratio).round.toInt

      i match {
        case _ if (i < 0) => rgbTable(0)
        case _ if (i > 255) => rgbTable(255)
        case _ => rgbTable(i)
      }
    }

    val bufImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    val cursor = Views.iterable(imgPlus).cursor

//    while (cursor.hasNext) {
//      val p = cursor.next.asInstanceOf[PixelType].getRealFloat
//      bufImage.setRGB(cursor.getIntPosition(0), cursor.getIntPosition(1), levelToColor(p))
//    }

    bufImage
  }

}