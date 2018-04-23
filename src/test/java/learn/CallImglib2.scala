package learn

import edu.umro.ImageUtil.FindWorstPixels
import com.pixelmed.dicom.AttributeList
import net.imglib2.img.Img
import net.imglib2.`type`.numeric.integer.UnsignedShortType
import io.scif.img.SCIFIOImgPlus
import ij.gui.NewImage
import ij.ImagePlus
import ij.plugin.PGM_Reader
import ij.process.ShortProcessor
import ij.ImageStack
import com.pixelmed.dicom.TagFromName
import io.scif.img.ImgOpener
import java.awt.image.BufferedImage
import net.imglib2.util.Intervals
import net.imglib2.Interval
import net.imglib2.view.Views
import java.awt.Color
import net.imglib2.`type`.numeric.RealType
import scala.collection.JavaConverters._
import java.util.{ Iterator => JIterator }
import java.io.File
import javax.imageio.ImageIO

object CallImglib2 {

  def attributeListToImageStack(attributeList: AttributeList): Option[ImageStack] = {
    val width = attributeList.get(TagFromName.Columns).getIntegerValues()(0)
    val height = attributeList.get(TagFromName.Rows).getIntegerValues()(0)
    val pixels = attributeList.getPixelData.getShortValues
    val ip = new ShortProcessor(width, height, pixels, null)
    val img = NewImage.createShortImage("hey", width, height, 1, 0)
    val stack: ImageStack = new ImageStack(width, height);
    stack.addSlice("hey", ip);
    Some(stack)
  }

  def readDicomFile(dicomFileName: String): Either[String, SCIFIOImgPlus[_]] = {
    try {
      Right((new ImgOpener).openImgs(dicomFileName).get(0))
    } catch {
      case t: Throwable => {
        Left("Unexpected error: " + t.toString)
        null
      }
    }
  }

  private type T = RealType[_]

  case class MinMax(min: Float, max: Float) {
    def this() = this(Float.MaxValue, Float.MinValue)

    def extreme(v: Float) = {
      new MinMax(Math.min(min, v), Math.max(max, v))
    }
  }

  def minMaxValuesOf(imgPlus: SCIFIOImgPlus[_]): MinMax = {
    val cursor = Views.iterable(imgPlus).cursor.asScala.asInstanceOf[Iterator[T]]
    val minMax = cursor.foldLeft(new MinMax)((mm, v) => mm.extreme(v.getRealFloat))
    minMax
  }

  def imgPlusToBufferedImage(imgPlus: SCIFIOImgPlus[_], color: Color): BufferedImage = {
    val width = imgPlus.getImg.dimension(0).toInt
    val height = imgPlus.getImg.dimension(1).toInt
    val minMax = minMaxValuesOf(imgPlus)

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

    while (cursor.hasNext) {
      val p = cursor.next.asInstanceOf[T].getRealFloat
      bufImage.setRGB(cursor.getIntPosition(0), cursor.getIntPosition(1), levelToColor(p))
    }

    bufImage
  }

  def main(args: Array[String]): Unit = {

    val fileName = "src\\test\\resources\\TestFindWorstPixels.dcm"

    val imgPlus = readDicomFile(fileName).right.get
    val minMax = minMaxValuesOf(imgPlus)
    println("minMax: " + minMax)

    val bufImg = imgPlusToBufferedImage(imgPlus, Color.CYAN)
    val pngName = "target\\foo.png"
    val pngFile = new File(pngName)
    pngFile.delete
    ImageIO.write(bufImg, "png", pngFile)

    println("done")

  }

}