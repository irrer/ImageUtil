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

class TestLocateRidge extends FlatSpec with Matchers {

  val outDir = new File("""target\TestLocateRidge""")
  Utility.deleteFileTree(outDir)
  outDir.mkdirs

  def getArray: IndexedSeq[IndexedSeq[Float]] = {
    val dir = new File("""src\test\resources""")
    val dicomFile = new File(dir, "TestLocateRidge.dcm")
    val al = new AttributeList
    al.read(dicomFile)
    val dicomImage = new DicomImage(al)
    val width = 18
    val height = 22
    val zoom = 10
    val array = dicomImage.getSubArray(new Rectangle(452 + 0, 440, width, height))
    array
  }

  "measured ridge" should "be close calculated ridge" in {

    val array = getArray

    val rectangle = new Rectangle2D.Double(0, 0, array.head.size, array.size)
    val zappedImage = LocateRidge.testZapUnusedPixels(array, rectangle)
    val zappedFile = new File(outDir, "zapped.png")
    println("Creating file with unused pixels zapped: " + zappedFile.getAbsolutePath)
    ImageUtil.writePngFile(zappedImage, zappedFile)

    val vertLineImage = LocateRidge.testLocateVerticalBuf(array, rectangle)
    val vertLineFile = new File(outDir, "vertLine.png")
    println("Creating file with unused pixels vertLine: " + vertLineFile.getAbsolutePath)
    ImageUtil.writePngFile(vertLineImage, vertLineFile)

    val center = LocateRidge.locateVertical(array, rectangle)
    println("center: " + center)

    true should be(true)
  }

}