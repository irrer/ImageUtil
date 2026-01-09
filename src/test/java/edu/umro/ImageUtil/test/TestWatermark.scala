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

package edu.umro.ImageUtil.test

import edu.umro.util.Utility
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.Watermark
import edu.umro.ScalaUtil.FileUtil
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.io.File
import javax.imageio.ImageIO

class TestWatermark extends FlatSpec with Matchers {
  val inDir = new File("""src/test/resources/Watermark""")
  val outDir = new File("target/TestWatermark")
  Utility.deleteFileTree(outDir)
  outDir.mkdirs

  "watermark" should "be coarse image" in {

    val watermarkFile = new File(inDir, "watermark.png")
    val image = ImageIO.read(watermarkFile)
    val watermark = new Watermark(image, false, false, 10, 30)

    val fileList = FileUtil.listFiles(inDir).filter(_.getName.toLowerCase.endsWith(".png"))

    def doFile(f: File): Unit = {
      val img = ImageIO.read(f)
      watermark.mark(img)
      val outFile = new File(outDir, f.getName)
      println("Writing watermarked file to " + outFile.getAbsolutePath)
      ImageUtil.writePngFile(img, outFile)
    }

    fileList.foreach(doFile)

    true should be(true)
  }

}
