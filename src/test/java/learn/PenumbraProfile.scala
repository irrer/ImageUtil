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

import java.io.File
import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import java.awt.Color
import javax.imageio.ImageIO
import edu.umro.ImageUtil.ImageUtil
import java.awt.Rectangle

object PenumbraProfile {

  def showPenumbrProfile = {
    val file = new File("""D:\tmp\aqa\Phase2\AQA-master\RI.$JM_AQA_phase2_v000.MV_0_0a.dcm""")
    val al = new AttributeList
    al.read(file)
    val image = new DicomImage(al)

    def horizontal = {
      val y = image.height / 2
      for (x <- (0 until image.width)) println(image.get(x, y))
    }

    def vertical = {
      val x = image.width / 2
      for (y <- (0 until image.height)) println(image.get(x, y))
    }

    horizontal
    println("\n\n------------------------------------------------\n\n")
    vertical

  }

  private def showBadPixel(bad: DicomImage.PixelRating, image: DicomImage) = {
    val sub = image.getSubimage(new Rectangle(bad.x - 2, bad.y - 2, 5, 5))
    println("        " + bad)
    for (y <- 0 until 5) {
      print("            ")
      for (x <- 0 until 5) print(sub.get(x, y).formatted("%8.1f"))
      println
    }
  }

  def dumpFile(file: File) {
    try {
      val al = new AttributeList
      al.read(file)
      if (al.isImage) {
        val image = new DicomImage(al)
        val badPixelList = image.identifyBadPixels(100, 1.0, 10.0, 5, 10.0)
        println("File " + file + " Number of bad pixels: " + badPixelList.size)
        badPixelList.map(bad => showBadPixel(bad, image))
        val bufImage = if (true) {
          val goodImage = image.correctBadPixels(badPixelList, 5)
          goodImage.toBufferedImage(Color.green)
        } else {
          image.toBufferedImage(Color.green)
        }
        badPixelList.map(w => ImageUtil.annotatePixel(bufImage, w.x, w.y, w.x + ", " + w.y, true))
        val imageFileName = file.getName.replaceAll(".dcm$", ".png").replaceAll(".DCM$", ".png")
        val imageFile = new File(file.getParentFile, imageFileName)
        imageFile.delete
        ImageIO.write(bufImage, "png", imageFile)
        println("Wrote file " + imageFile)
      } else println("Ignoring file " + file)
    } catch {
      case t: Throwable => println("Error.  Ignoring file " + file)
    }
  }

  def dumpDir(dir: File) = {
    dir.listFiles.map(f => dumpFile(f))
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis
    dumpDir(new File("""D:\tmp\aqa\Phase2\$JM_AQA_phase2_v000"""))
    println("Done.  Elapsed ms: " + (System.currentTimeMillis - start).toString)
  }

}