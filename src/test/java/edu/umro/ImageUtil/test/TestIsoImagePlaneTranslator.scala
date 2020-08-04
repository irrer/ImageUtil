package edu.umro.ImageUtil.test;

import edu.umro.ScalaUtil.Trace
import java.io.File
import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import java.awt.Rectangle
import edu.umro.util.Utility
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator

object TestIsoImagePlaneTranslator {

  val inDir = new File("""src\test\resources\TestIsoImagePlaneTranslator""")

  def check(name: String, cur: Double, old: Double) {
    val diff = cur - old
    def fmt(d: Double) = d.formatted("%14.8f")
    println(name.formatted("%-16s") + " cur: " + fmt(cur) + "     old: " + fmt(old) + "    diff: " + fmt(diff))
    if (diff.abs > 0.00001) {
      Thread.sleep(100)
      throw new RuntimeException("diff is too large")
    }
  }

  def testFile(file: File) = {
    val al = new AttributeList
    al.read(file)
    println("\nTesting file " + file.getAbsolutePath)

    val ipt = new IsoImagePlaneTranslator(al)
    val iptOld = new IsoImagePlaneTranslatorOld(al)

    check("iso2PixCoordX", ipt.iso2PixCoordX(0), iptOld.iso2PixCoordX(0))

    for (v <- (-10000 to 10000)) {
      val d = v / 10.0
      check("iso2PixDistX", ipt.iso2PixDistX(d), iptOld.iso2PixDistX(d))
      check("iso2PixCoordX", ipt.iso2PixCoordX(d), iptOld.iso2PixCoordX(d))
      check("iso2PixDistY", ipt.iso2PixDistY(d), iptOld.iso2PixDistY(d))
      check("iso2PixCoordY", ipt.iso2PixCoordY(d), iptOld.iso2PixCoordY(d))

      check("pix2IsoDistX", ipt.pix2IsoDistX(d), iptOld.pix2IsoDistX(d))
      check("pix2IsoCoordX", ipt.pix2IsoCoordX(d), iptOld.pix2IsoCoordX(d))
      check("pix2IsoDistY", ipt.pix2IsoDistY(d), iptOld.pix2IsoDistY(d))
      check("pix2IsoCoordY", ipt.pix2IsoCoordY(d), iptOld.pix2IsoCoordY(d))
    }
  }

  def main(args: Array[String]): Unit = {
    inDir.listFiles.toSeq.map(f => testFile(f))
    println("all tests passed")
  }

}