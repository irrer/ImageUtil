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

import com.pixelmed.dicom.{AttributeList, TagFromName}
import edu.umro.ImageUtil.DicomImage
import edu.umro.ScalaUtil.DicomUtil

import java.io.File

object ConstructIdealCollimatorCentering {

  def readDicomFile(file: File): AttributeList = {
    val al = new AttributeList
    al.read(file)
    al
  }

  def showProfile(file: File): Unit = {
    val al = readDicomFile(file)
    val dicomImage = new DicomImage(al)

    val mid = dicomImage.Rows / 2
    val text = (0 until dicomImage.Columns).map(c => dicomImage.get(c, mid).formatted("%6.0f")).mkString("\n    ")
    println("======= profile of " + file.getName + " =========\n    " + text + "============================")
  }

  // -------------------------------------------------------

  val dir = new File("D:\\tmp\\aqa\\Phase2\\DICOM\\umich\\8perfect")

  val file090 = new File(dir, "J10G0C90-6X.dcm")
  val file270 = new File(dir, "J10G0C270-6X.dcm")

  val left = 368
  val right: Int = 1190 - (left + 1)
  val top: Int = left
  val bottom: Int = right

  def idealize(file: File): Unit = {
    val al = readDicomFile(file)

    val dicomImage = new DicomImage(al)
    val height = dicomImage.height
    val width = dicomImage.width

    val min = 340.toShort
    val max = 27369.toShort
    val pixAttr = al.get(TagFromName.PixelData)
    val pixels = pixAttr.getShortValues

    for (y <- 0 until height; x <- 0 until width) {
      val p = if ((y > top) && (y < bottom) && (x > left) && (x < right)) max else min
      val i = (y * width) + x
      pixels(i) = p
    }

    val outFile = new File(dir, "I" + file.getName)
    outFile.delete
    DicomUtil.writeAttributeListToFile(al, outFile, "Idealized")
    println("wrote file to " + outFile.getAbsolutePath)
  }

  def main(args: Array[String]): Unit = {

    //   showProfile(file090)

    idealize(file090)
    idealize(file270)

  }

}
