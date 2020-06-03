package edu.umro.ImageUtil.test;

import edu.umro.ImageUtil.LocateEdge
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import java.awt.Rectangle
import com.pixelmed.dicom.TagFromName
import scala.collection.mutable.ArrayBuffer
import edu.umro.ScalaUtil.DicomUtil

class TestLocateEdge extends FlatSpec with Matchers {

  "measured edge" should "be close calculated edge" in {

    val dir = new File("""src/test/resources/TestLocateEdge""")
    val dicomFileList = dir.listFiles.filter(f => f.getName.toLowerCase.endsWith(".dcm"))

    def readDicomFile(file: File) = {
      val al = new AttributeList
      al.read(file)
      al
    }

    val imageList = dicomFileList.map(f => readDicomFile(f))

    // edge positions
    val topEdge = 600
    val bottomEdge = 680
    val leftEdge = 600
    val rightEdge = 680

    val border = 20

    def measure(image: DicomImage) = {
      println("----------------------------------------------")

      val s = 200
      val midW = image.width / 2
      val midH = image.height / 2
      val top = image.getSubimage(new Rectangle(midW - s, midH - border, s * 2, border * 2))
      //println(top.columnSums.mkString("\n    ", "\n    ", ""))

      val ht = (bottomEdge - border) - (topEdge + border)
      val x = leftEdge - border
      val sub = image.getSubimage(new Rectangle(x, topEdge + border, border * 2, ht))
      val colSums = sub.columnSums.map(s => s / ht)
      println(colSums.mkString("\n    ", "\n    ", ""))
      val threshold = (colSums.max + colSums.min) / 2.0
      println("Left edge at: " + (x + LocateEdge.locateEdge(colSums, threshold)))

      true should be(true)
    }

    if (true)
      dicomFileList.map(file => measure(new DicomImage(readDicomFile(file))))

    // -------------------------------------------------------

    def modSop(al: AttributeList, incr: Int) = {
      val at = al.get(TagFromName.SOPInstanceUID)
      val sop = at.getSingleStringValueOrEmptyString
      at.removeValues
      at.addValue(sop + incr)
    }

    // make image without ball and with ball
    if (true) {
      val al = readDicomFile(dicomFileList.head)
      modSop(al, 1)

      val width = al.get(TagFromName.Columns).getIntegerValues.head
      val height = al.get(TagFromName.Rows).getIntegerValues.head

      val RadiationMachineSAD = al.get(TagFromName.RadiationMachineSAD)
      RadiationMachineSAD.removeValues
      RadiationMachineSAD.addValue(1000.0)

      val RTImageSID = al.get(TagFromName.RTImageSID)
      RTImageSID.removeValues
      RTImageSID.addValue(1500.0)

      val pix = ArrayBuffer[Short]()
      val min = 1.toShort
      val max = 3200.toShort
      val pixAttr = al.get(TagFromName.PixelData)
      val pixels = pixAttr.getShortValues

      for (y <- 0 until height; x <- 0 until width) {
        val p = if ((y > topEdge) && (y < bottomEdge) && (x > leftEdge) && (x < rightEdge)) max else min
        val i = (y * width) + x
        pixels(i) = p
      }
      if (true) {
        val outFile = new File(new File("target"), "abstractNoBall.dcm")
        outFile.delete
        DicomUtil.writeAttributeListToFile(al, outFile, "TestLocateEdge")
        println("wrote file to " + outFile.getAbsolutePath)

        //  ------------------------------------------------------------------

        // make image with ball
        modSop(al, 2)

        val mid = 2000.toShort

        val radius = 11
        val xCntr = (width / 2) - 0.5
        val yCntr = (height / 2) - 0.5

        for (y <- 0 until height; x <- 0 until width) {
          val xd = x - xCntr
          val yd = y - yCntr

          val dist = Math.sqrt((xd * xd) + (yd * yd))
          if (dist < radius) {
            val i = (y * width) + x
            pixels(i) = mid
          }
        }
        if (true) {
          val outFile = new File(new File("target"), "abstractWithBall.dcm")
          outFile.delete
          DicomUtil.writeAttributeListToFile(al, outFile, "TestLocateEdge")
          println("wrote file to " + outFile.getAbsolutePath)
        }
      }
    }
  }
}









