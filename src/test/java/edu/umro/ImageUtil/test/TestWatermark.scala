package edu.umro.ImageUtil.test;

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.awt.geom.Rectangle2D
import edu.umro.ImageUtil.LocateRidge
import edu.umro.ScalaUtil.Trace
import java.io.File
import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import java.awt.Rectangle
import edu.umro.util.Utility
import edu.umro.ImageUtil.ImageUtil
import javax.imageio.ImageIO
import edu.umro.ImageUtil.Watermark

class TestWatermark extends FlatSpec with Matchers {
  val outDir = new File("""target\TestWatermark""")
  Utility.deleteFileTree(outDir)
  outDir.mkdirs

  "watermark" should "be coarse image" in {

    val inFile = new File("""D:\pf\eclipse\workspaceOxygen\ImageUtil\src\test\resources\watermark.png""")
    val image = ImageIO.read(inFile)

    val watermark = new Watermark(image, true, false, 10)

    watermark.mark(image)

    val outFile = new File(outDir, "watermark.png")
    println("Writing watermarked file to " + outFile.getAbsolutePath)
    ImageUtil.writePngFile(image, outFile)

    true should be(true)
  }

}