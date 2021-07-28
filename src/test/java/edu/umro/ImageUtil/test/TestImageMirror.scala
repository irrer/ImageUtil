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

package edu.umro.ImageUtil.test;

import edu.umro.ImageUtil.LocateEdge
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.awt.image.BufferedImage
import edu.umro.ImageUtil.ImageUtil
import javax.swing.JFrame
import java.awt.FlowLayout
import javax.swing.JLabel
import javax.swing.ImageIcon
import edu.umro.ImageUtil.ImageText
import java.awt.RenderingHints
import java.io.File
import javax.imageio.ImageIO
import edu.umro.util.Utility

class TestImageMirror extends FlatSpec with Matchers {

  "mirror" should "flip" in {
    TestImageMirror.perform
  }
}

object TestImageMirror {

  val outDir = new File("""target\TestMirror""")

  //  private def showBufImg(bufImg: BufferedImage) = {
  //    val frame = new JFrame
  //    frame.getContentPane.setLayout(new FlowLayout);
  //    frame.getContentPane.add(new JLabel(new ImageIcon(bufImg)))
  //    frame.pack
  //    frame.setVisible(true)
  //  }

  private def testImage(file: File) = {
    println("testing mirror on file " + file.getAbsolutePath)
    val original = ImageIO.read(file)
    val horz = ImageUtil.mirrorHorizontally(original)
    val vert = ImageUtil.mirrorVertically(original)
    val horzThenHorz = ImageUtil.mirrorHorizontally(horz)
    val vertThenVert = ImageUtil.mirrorVertically(vert)
    val vertThenHorz = ImageUtil.mirrorHorizontally(vert)
    val horzThenVert = ImageUtil.mirrorVertically(horz)

    val baseName = {
      val i = file.getName.lastIndexOf(".")
      file.getName.take(i + 1)
    }

    ImageUtil.writePngFile(original, new File(outDir, file.getName))
    ImageUtil.writePngFile(horz, new File(outDir, baseName + "_horz.png"))
    ImageUtil.writePngFile(vert, new File(outDir, baseName + "_vert.png"))
    ImageUtil.writePngFile(horzThenHorz, new File(outDir, baseName + "_horzThenHorz.png"))
    ImageUtil.writePngFile(vertThenVert, new File(outDir, baseName + "_vertThenVert.png"))
    ImageUtil.writePngFile(vertThenHorz, new File(outDir, baseName + "_vertThenHorz.png"))
    ImageUtil.writePngFile(horzThenVert, new File(outDir, baseName + "_horzThenVert.png"))

    println("Wrote mirrored image files to " + outDir.getAbsolutePath)

    //    showBufImg(original)
    //    showBufImg(r090)
    //    showBufImg(r180)
    //    showBufImg(r270)
    //    showBufImg(r360)
    //    Thread.sleep(10 * 1000)
  }

  private def perform = {
    val list = (new File("""src\test\resources\TestMirror""")).listFiles

    outDir.mkdirs
    outDir.listFiles.map(Utility.deleteFileTree)

    list.map(testImage)
  }

  def main(args: Array[String]): Unit = {
    println("starting")

    perform

    System.exit(0)
  }

}