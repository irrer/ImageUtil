/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.umro.ImageUtil

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.OtherWordAttribute
import com.pixelmed.dicom.SOPClass
import com.pixelmed.dicom.TagFromName
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil

import java.io.File
import java.nio.ByteBuffer
import java.nio.ByteOrder
import javax.imageio.ImageIO

object DicomFromDeepColorPng {

  private val percentToDrop = 0.02

  /**
    * Safely get a list of files in a directory.  On failure, return an empty list.
    */
  private def listFiles(dir: File): List[File] = {
    try {
      dir.listFiles.toList
    } catch {
      case _: Throwable => List[File]()
    }
  }

  private def listRegularFiles(file: File): Seq[File] = {
    if (file.isFile) Seq(file)
    else {
      val fList = listFiles(file)
      fList.flatMap(f => listRegularFiles(f))
    }
  }

  /**
    * Make a map that, given a color, returns the deep color pixel value to use.
    * @param minimumV Minimum pixel value.
    * @param maximumV Maximum pixel value.
    * @return Function for color --> DICOM
    */
  private def makeDeepColorMap(minimumV: Float, maximumV: Float): Map[Int, Int] = {

    val range = (maximumV - minimumV).abs

    /**
      * Given a pixel value, return an RGB color
      */
    def p2Color(pixel: Float): Int = {
      val scaledPixel = {
        {
          (pixel - minimumV) / range
        } match {
          case p if p < 0 => 0
          case p if p > 1 => 1
          case p          => p
        }
      }

      val a = (1 - scaledPixel) / 0.2
      val X = Math.floor(a).toInt
      val Y = Math.floor(255 * (a - X)).toInt

      val rgb = X match {
        case 0 => (255, Y, 0)
        case 1 => (255 - Y, 255, 0)
        case 2 => (0, 255, Y)
        case 3 => (0, 255 - Y, 255)
        case 4 => (Y, 0, 255)
        case 5 => (255, 0, 255)
        case _ => (255, 0, 255) // pixel outside range
      }
      (rgb._1 << 8) + (rgb._2 << 16) + rgb._3
    }

    val step = range / 1276
    val rgbMap = (0 until range.round)
      .map(p => {
        val pix = (p * step) + minimumV
        (p2Color(pix), pix.round)
      })
      .toMap

    rgbMap
  }

  private def isImageStg(al: AttributeList) = {
    val c = al.get(TagFromName.SOPClassUID).getSingleStringValueOrEmptyString
    SOPClass.isImageStorage(c)
  }

  private def usage(msg: String): Unit = {
    println(msg)
    System.exit(1)
  }

  def main(args: Array[String]): Unit = {
    val list = args.flatMap(fileName => listRegularFiles(new File(fileName)))

    val pngFile = list.find(f => f.getName.toLowerCase().endsWith(".png"))
    val dcmFile = list.find(f => f.getName.toLowerCase().endsWith(".dcm"))

    if ((list.length != 2) || pngFile.isEmpty || dcmFile.isEmpty) {
      usage("Must specify exactly two files, a *.dcm and a *.png")
    }

    val al = new AttributeList
    al.read(dcmFile.get)

    if (!isImageStg(al)) {
      usage("DICOM file must be an image file (e.g. RTIMAGE, CT)")
    }

    val bi = ImageIO.read(pngFile.get)
    val dicomImage = new DicomImage(al)
    val minMax = dicomImage.dropOutlierPixels(percentToDrop)

    val rgbMap = makeDeepColorMap(minMax._1, minMax._2)

    def splitRgb(rgb: Int) = {
      val a = rgb & 0xffffff
      Seq((a >> 16) & 0xff, (a >> 8) & 0xff, a & 0xff)
    }

    val rgbList = rgbMap.keys.map(splitRgb)

    if (true) {
      val s = rgbMap.values.toSeq.sorted
      println(s.take(10).mkString("  "))
      println(s.takeRight(10).mkString("  "))
    }

    def rgbDiff(a: Seq[Int], b: Seq[Int]): Int = {
      a.zip(b).map(ab => (ab._1 - ab._2).abs).sum
    }

    /**
      * Given a color, return the pixel value it corresponds to.  If an exact match is not found,
      * then find the most similar color using the sum of the difference of each of the red, green,
      * blue components.
      *
      * @param color Find for this color.
      * @return pixel value that best matches.
      */
    def mapper(color: Int): Float = {
      val j = color & 0xffffff
      rgbMap.get(j) match {
        case Some(pix) =>
          pix
        case _ =>
          val rgbColor = splitRgb(color)
          val colors = rgbList.minBy(rgb => rgbDiff(rgb, rgbColor))
          val rgb = (colors.head << 16) + (colors(1) << 8) + colors(2)
          rgbMap(rgb)
      }
    }

    def pixRow(y: Int): IndexedSeq[Float] = {
      for (x <- 0 until bi.getWidth()) yield {
        mapper(bi.getRGB(x, y))
      }
    }

    val pix2D = for (y <- 0 until bi.getHeight()) yield pixRow(y)

    val pix1d = pix2D.flatten
      .map(p => if (p > 65535) 65535 else p) // fixes spurrious pixels.  This is a bit a of hack.
      .map(pix => {
        val pr = pix.round
        val hi = ((pr >> 8) & 0xff).toByte
        val lo = (pr & 0xff).toByte
        val array = Array(lo, hi)
        ByteBuffer.wrap(array).order(ByteOrder.LITTLE_ENDIAN).getShort()
      })
      .toArray

    val pixelAttr = al.get(TagByName.PixelData).asInstanceOf[OtherWordAttribute]
    pixelAttr.setValues(pix1d)

    val newFile = {
      val name = dcmFile.get.getName.dropRight(4) + "_" + dcmFile.get.getName.takeRight(4)
      new File(dcmFile.get.getParentFile, name)
    }

    DicomUtil.writeAttributeListToFile(al, newFile, "FromDeepColor")
    println("Wrote file " + newFile.getAbsolutePath)
  }
}
