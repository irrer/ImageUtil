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
import org.opensourcephysics.numerics.CubicSpline

object LocateEdgeImageDoc {

  private val inDir = new File("""src\test\resources\LocateEdgeImageDoc""")

  private def read(name: String): AttributeList = {
    val file = new File(inDir, name)
    val al = new AttributeList
    al.read(file)
    al
  }

  def main(args: Array[String]): Unit = {

    val attrList = read("J10G0C270-6X.dcm")

    val dicomImage = new DicomImage(attrList)
    val x = 773
    val y = 416
    val width = 893
    val height = 357
    val edgeImage = dicomImage.getSubimage(new Rectangle(x, y, width, height))
    val profile = edgeImage.columnSums.map(p => p.toDouble).toArray

    val granularity = 10

    //println("Integer values:\n" + (0 to profile.size by granularity).map(i => i + "," + profile(i)).mkString("\n"))
    println("-------")

    val cubicSpline = new CubicSpline(profile.indices.toArray.map(i => i.toDouble), profile)

    def toGraph(x: Int) = {
      val g2 = granularity / 2
      val i = ((x - g2) / granularity) * granularity
      println(x + "," + profile(i) + "," + cubicSpline.evaluate(x))
    }

    (0 to (profile.size)).map(x => toGraph(x))

  }

}