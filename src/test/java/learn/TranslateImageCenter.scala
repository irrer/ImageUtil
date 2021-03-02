package learn

import java.io.File
import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage

import java.awt.Color
import javax.imageio.ImageIO
import edu.umro.ImageUtil.ImageUtil

import java.awt.Rectangle
import com.pixelmed.dicom.TagFromName

import java.io.FileOutputStream
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeFactory
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator

object TranslateImageCenter {

  def main(args: Array[String]): Unit = {

    val al = new AttributeList

    def add(tag: AttributeTag, value: String): Unit = {
      val at = AttributeFactory.newAttribute(tag)
      at.addValue(value)
      al.put(at)
    }

    def addMult(tag: AttributeTag, valueList: Seq[Double]): Unit = {
      val at = AttributeFactory.newAttribute(tag)
      valueList.map(value => at.addValue(value))
      al.put(at)
    }

    if (false) {
      val imageSize = 16
      val pixSize = 0.5

      addMult(TagByName.XRayImageReceptorTranslation, Seq(2.0, 3.0, 500.0))
      add(TagFromName.Rows, imageSize.toString)
      add(TagFromName.Columns, imageSize.toString)
      add(TagByName.RTImageSID, "1500.0")
      add(TagByName.RadiationMachineSAD, "1000.0")
      addMult(TagByName.ImagePlanePixelSpacing, Seq(pixSize, pixSize))
      val o = ((imageSize / 2) * pixSize) - (pixSize / 2)
      addMult(TagByName.RTImagePosition, Seq(-o, o))
    } else {
      val real = new AttributeList
      real.read(new File("""D:\tmp\0001.dcm"""))
      def cpy(tag: AttributeTag) = al.put(real.get(tag))
      cpy(TagByName.XRayImageReceptorTranslation)
      cpy(TagFromName.Rows)
      cpy(TagFromName.Columns)
      cpy(TagByName.RTImageSID)
      cpy(TagByName.RadiationMachineSAD)
      cpy(TagByName.ImagePlanePixelSpacing)
      cpy(TagByName.RTImagePosition)
    }

    println("al:\n" + DicomUtil.attributeListToString(al))

    val trans = new IsoImagePlaneTranslator(al)

    println("caxCenter_pix: " + trans.caxCenter_pix)
    println("caxCenter_iso: " + trans.caxCenter_iso)
    println("old:")
//    println("isoCenter to pix: " + trans.iso2Pix(trans.isoCenter))
//    println("isoCenter: " + trans.isoCenter)

    println("Done.")
  }

}