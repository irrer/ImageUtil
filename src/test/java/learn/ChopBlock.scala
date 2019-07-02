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

object ChopBlock {

  private val inDir = new File("""D:\tmp\aqa\CBCT\MQATX1OBIQA2019Q3\output\TX1\TX1_CT_2_5""")

  private val outDir = new File("""D:\tmp\aqa\CBCT\centerImages""")

  private def read(file: File): AttributeList = {
    val al = new AttributeList
    al.read(file)
    al
  }

  private def center(al: AttributeList, name: String) {
    val img = new DicomImage(al)
    val cx = img.width / 2
    val cy = img.height / 2
    println("name: " + name)
    for (x <- (cx - 2) until (cx + 2); y <- (cy - 2) until (cy + 2)) yield {
      println("    x,y: " + x + ", " + y + " : " + img.get(x, y))
    }
  }

  def getPosn(al: AttributeList): Double = al.get(TagFromName.ImagePositionPatient).getDoubleValues()(2)

  val range = 30.0 // range in mm from center

  def showPix(di: DicomImage) = {
    println
    for (y <- (0 until di.height)) {
      for (x <- (0 until di.width)) {
        print(di.get(x, y).toInt.formatted("%4d  "))
      }
      println
    }
    println
  }

  def main(args: Array[String]): Unit = {

    outDir.mkdirs
    outDir.listFiles.map(f => f.delete)
    Thread.sleep(500)

    val alList = inDir.listFiles.map(f => read(f)).sortBy(al => getPosn(al))
    val centerSlices = {
      val min = getPosn(alList.head)
      val max = getPosn(alList.last)
      val mid = (min + max) / 2
      alList.filter(al => (getPosn(al) - mid).abs < range)
    }

    println("number of slices: " + centerSlices.size)

    val pixSpacingX = centerSlices.head.get(TagFromName.PixelSpacing).getDoubleValues()(0)
    val pixSpacingY = centerSlices.head.get(TagFromName.PixelSpacing).getDoubleValues()(1)

    val pixSpanX = (range / pixSpacingX)
    val pixSpanY = (range / pixSpacingY)

    val rows = centerSlices.head.get(TagFromName.Rows).getIntegerValues()(0)
    val columns = centerSlices.head.get(TagFromName.Columns).getIntegerValues()(0)

    val pixX = ((rows - pixSpanX) / 2.0).round.toInt
    val pixY = ((rows - pixSpanY) / 2.0).round.toInt

    val rect = new Rectangle(pixX, pixY, pixSpanX.round.toInt, pixSpanY.round.toInt)

    val imageList = centerSlices.map(al => new DicomImage(al)).map(di => di.getSubimage(rect))

    val minLevel = 1000.toFloat // imageList.map(img => img.minPixelValue).min
    val maxLevel = imageList.map(img => img.maxPixelValue).max

    println("minLevel: " + minLevel + "    maxLevel: " + maxLevel)

    val bufImageList = centerSlices.map(al => new DicomImage(al)).map(di => di.getSubimage(rect)).map(di => di.toDeepColorBufferedImage(minLevel, maxLevel))

    def writePng(i: Int) = {
      val name = (i + 1).formatted("%3d.png")
      val pngFile = new File(outDir, name)
      ImageIO.write(bufImageList(i), "png", new FileOutputStream(pngFile))
    }

    bufImageList.indices.map(i => writePng(i))

    imageList.map(img => showPix(img))

    println("Done.  Files written to " + outDir.getAbsolutePath)
  }

}