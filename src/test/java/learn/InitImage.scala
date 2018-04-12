package learn

import io.scif.img.ImgOpener
import io.scif.img.SCIFIOImgPlus
import net.imglib2.`type`.numeric.integer.UnsignedByteType
import net.imglib2.`type`.numeric.integer.UnsignedShortType

object InitImage {
  def main(args: Array[String]): Unit = {
    println("Starting")
    //val fileName = """src\test\data\T2_Open_AQA_phase2.dcm.png"""
    val fileName = """D:\tmp\aqa\Phase2\AQA-master\RI.$JM_AQA_phase2_v000.MV_243_0a.dcm"""

    val imgOpener = new ImgOpener
    val imgList = imgOpener.openImgs(fileName)
    println("image has been read")
    val img: SCIFIOImgPlus[_] = imgList.get(0)
    val meta = img.getImageMetadata
    val axesLengths = meta.getAxesLengths
    val iter = img.iterator

    val randAcc = img.randomAccess
    randAcc.setPosition(5, 0)
    randAcc.setPosition(12, 1)
    val pix = randAcc.get
    println("pix: " + pix)

    var max = 0
    var total = 0
    var count = 0
    while (iter.hasNext) {
      //val nxt: UnsignedByteType = iter.next.asInstanceOf[UnsignedByteType]
      val nxt = iter.next.asInstanceOf[UnsignedShortType]
      val i = nxt.get
      if (i > max)
        max = i
      total = total + i
      count = count + 1
      //println("nxt: " + nxt)
    }
    println("count: " + count)
    println("total: " + total)
    println("max: " + max)

    println("Done")
    System.exit(0)
  }

}