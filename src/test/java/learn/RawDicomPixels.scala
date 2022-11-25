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

import java.io.File

object RawDicomPixels {

  private val inDir = new File("""D:\tmp\aqa\raw""")

  private def read(name: String): AttributeList = {
    val file = new File(inDir, name)
    val al = new AttributeList
    al.read(file)
    al
  }

  private def center(al: AttributeList, name: String): Unit = {
    val img = new DicomImage(al)
    val cx = img.width / 2
    val cy = img.height / 2
    println("name: " + name)
    for (x <- (cx - 2) until (cx + 2); y <- (cy - 2) until (cy + 2)) yield {
      println("    x,y: " + x + ", " + y + " : " + img.get(x, y))
    }
  }

  def main(args: Array[String]): Unit = {

    val wedge = read("wedge.dcm")
    val flood = read("flood.dcm")

    center(wedge, "wedge")
    center(flood, "flood")

  }

}
