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

class TestImageRotate extends FlatSpec with Matchers {

  "rotate90" should "rotate entire image 90 clockwise" in {
    TestImageRotate.perform
  }
}

object TestImageRotate {

  val outDir = new File("""target\TestRotate""")

  //  private def showBufImg(bufImg: BufferedImage) = {
  //    val frame = new JFrame
  //    frame.getContentPane.setLayout(new FlowLayout);
  //    frame.getContentPane.add(new JLabel(new ImageIcon(bufImg)))
  //    frame.pack
  //    frame.setVisible(true)
  //  }

  private def testImage(file: File) = {
    println("testing rotate on file " + file.getAbsolutePath)
    val original = ImageIO.read(file)
    val r090 = ImageUtil.rotate90(original)
    val r180 = ImageUtil.rotate180(original)
    val r270 = ImageUtil.rotate270(original)

    val baseName = {
      val i = file.getName.lastIndexOf(".")
      file.getName.take(i + 1)
    }

    ImageUtil.writePngFile(original, new File(outDir, file.getName))
    ImageUtil.writePngFile(r090, new File(outDir, baseName + "_r090.png"))
    ImageUtil.writePngFile(r180, new File(outDir, baseName + "_r180.png"))
    ImageUtil.writePngFile(r270, new File(outDir, baseName + "_r270.png"))

    println("Wrote rotated image files to " + outDir.getAbsolutePath)

    //    showBufImg(original)
    //    showBufImg(r090)
    //    showBufImg(r180)
    //    showBufImg(r270)
    //    showBufImg(r360)
    //    Thread.sleep(10 * 1000)
  }

  private def perform = {
    val list = (new File("""src\test\resources\TestRotate""")).listFiles

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