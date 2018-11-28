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

class TestLocateRidge extends FlatSpec with Matchers {

  def getArray: IndexedSeq[IndexedSeq[Float]] = {
    val dir = new File("""src\test\resources""")
    val dicomFile = new File(dir, "TestLocateRidge.dcm")
    val al = new AttributeList
    al.read(dicomFile)
    val dicomImage = new DicomImage(al)
    val width = 18
    val height = 22
    val zoom = 10
    val array = dicomImage.getSubArray(new Rectangle(452, 440, width, height))
    array
  }

  "measured ridge" should "be close calculated ridge" in {

    val array = getArray

    val center = LocateRidge.locateVertical(array, new Rectangle2D.Double(0, 0, array.head.size, array.size))
    println("center: " + center)
    true should be(true)
  }

}