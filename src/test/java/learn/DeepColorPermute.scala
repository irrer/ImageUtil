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

package learn

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage

import java.awt.image.{BufferedImage, RenderedImage}
import java.io.{ByteArrayOutputStream, File, FileOutputStream}
import javax.imageio.ImageIO

object DeepColorPermute {

  private def readDicom(f: File) = {
    val al = new AttributeList
    al.read(f)
    al
  }

  case class Shifty(name: String, red: Int, green: Int, blue: Int) {
    def fmt(i: Int): String = i.formatted("_%02d")
    val fullName: String = name + fmt(red) + fmt(green) + fmt(blue)
  }

  def writeBinaryFile(file: File, data: Array[Byte]): Unit =
    (writeBinaryFile _).synchronized({
      val fos = new FileOutputStream(file)
      fos.write(data)
      fos.flush()
      fos.close()
    })

  def writePng(im: RenderedImage, pngFile: File): Unit = {
    pngFile.delete
    val stream = new ByteArrayOutputStream
    ImageIO.write(im, "png", stream)
    writeBinaryFile(pngFile, stream.toByteArray)
  }

  var minLevel = 100000
  var maxLevel: Int = -1

  /**
    * Create a buffered image of this DICOM image using multiple colors to give a deeper color depth of 1276 than the standard 256.
    */
  def toDeepColor(image: DicomImage, shift: Shifty): BufferedImage = {

    val minimum = image.minPixelValue
    val maximum = image.maxPixelValue
    val width = image.width
    val height = image.height

    val range = maximum - minimum
    println("toDeepColor minimum: " + minimum)
    println("toDeepColor maximum: " + maximum)
    println("toDeepColor range: " + range)

    /**
      * Given a pixel value, return an RGB color
      */
    def p2Color(pixel: Float): Int = {

      val scaledPixel = {
        val p = pixel match {
          case _ if pixel < minimum => minimum
          case _ if pixel > maximum => maximum
          case _                    => pixel
        }
        (p - minimum) / range
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
      //  (rgb._1 << 16) + (rgb._2 << 8) + rgb._3
      val color = (rgb._1 << shift.red) + (rgb._2 << shift.green) + (rgb._3 << shift.blue)
      val level = rgb._1 + rgb._2 + rgb._3
      minLevel = Math.min(level, minLevel)
      maxLevel = Math.max(level, maxLevel)
      color
    }

    val bufImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    for (row <- 0 until height; col <- 0 until width) bufImage.setRGB(col, row, p2Color(image.get(row, col)))

    bufImage
  }

  val pixMap: List[Int] = FindDeepColorList.findDeepColor
  println("pixMap size: " + pixMap.size)

  def toDeepColor2(image: DicomImage): BufferedImage = {

    val minimum = image.minPixelValue
    val maximum = image.maxPixelValue
    val width = image.width
    val height = image.height

    val range = maximum - minimum
    val size = pixMap.size - 1

    /**
      * Given a pixel value, return an RGB color
      */
    def p2RGB(pixel: Float): Int = {
      val scaledPixel = (((pixel - minimum) / range) * size).floor.toInt
      pixMap(scaledPixel)
    }

    val bufImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    for (row <- 0 until height; col <- 0 until width) bufImage.setRGB(col, row, p2RGB(image.get(row, col)))

    bufImage
  }

  private def alToCorrected(al: AttributeList): DicomImage = {
    val rawImage = new DicomImage(al)
    val image = rawImage.correctBadPixels(rawImage.identifyBadPixels(400, 2.0, 10.0, 5, 10.0), 5)
    image
  }

  private val dirName = """D:\tmp\aqa\colorTest\png"""
  def dir = new File(dirName)

  /**
    * Correct bad pixels several times to get an average performance.
    */
  private def timeBadPixelCorrection(alList: Seq[AttributeList]): Unit = {
    for (_ <- 0 to 5) {
      val st = System.currentTimeMillis
      alList.map(al => alToCorrected(al))
      val el = System.currentTimeMillis - st
      println("timeBadPixelCorrection corrected elapsed time in ms: " + el + "    per image: " + (el.toDouble / alList.size))
    }
  }

  private def timeDeepColorRendering(list: Seq[DicomImage]): Unit = {
    for (_ <- 0 to 5) {
      val st = System.currentTimeMillis
      list.map(di => di.toDeepColorBufferedImage(0.2))
      val el = System.currentTimeMillis - st
      println("timeDeepColorRendering corrected elapsed time in ms: " + el + "    per image: " + (el.toDouble / list.size))
    }
  }

  private def timeDeepColor2Rendering(list: Seq[DicomImage]): Unit = {
    for (_ <- 0 to 5) {
      val st = System.currentTimeMillis
      list.map(di => toDeepColor2(di))
      val el = System.currentTimeMillis - st
      println("timeDeepColor2Rendering corrected elapsed time in ms: " + el + "    per image: " + (el.toDouble / list.size))
    }
  }

  def main(args: Array[String]): Unit = {

    if (true) {
      val al = new AttributeList
      al.read(new File("src/test/resources/TestFindWorstPixels/TestFindWorstPixels0.dcm"))
      val di = new DicomImage(al)
      val img = di.toDeepColorBufferedImage(0.01)
      writePng(img, new File("""D:\tmp\foo.png"""))
      println("Exiting ...")
      System.exit(0)
    }

    val start = System.currentTimeMillis
    val alList = new File("""D:\tmp\aqa\colorTest""").listFiles.filter(f => f.getName.toLowerCase.endsWith(".dcm")).map(f => readDicom(f)).toList

    //    dir.mkdirs
    //    dir.listFiles.map(f => f.delete)
    //    dir.delete
    //    dir.mkdirs

    println("Number of images: " + alList.size)

    if (false) {
      timeBadPixelCorrection(alList)
      System.exit(0)
    }

    val correctedList = alList.par.map(al => alToCorrected(al)).toList
    println("Corrections complete.  Elapsed ms: " + (System.currentTimeMillis - start))

    if (true) {
      println("Timing deep color rendering")
      timeDeepColorRendering(correctedList)
      //System.exit(0)
    }

    if (true) {
      println("Timing deep color 2 rendering")
      timeDeepColor2Rendering(correctedList)
    }

    //shiftPermutationList.map(shift => imagesToPng(correctedList, shift))

    //imagesToPng2(correctedList)

    //imagesTo768Png(correctedList)

    println("minLevel: " + minLevel)
    println("maxLevel: " + maxLevel)
    val elapsed = System.currentTimeMillis - start
    println("Done.  Elapsed ms: " + elapsed)
  }

}
