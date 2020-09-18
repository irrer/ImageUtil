package edu.umro.ImageUtil.test

import java.io.File
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ImageUtil.DicomVolume
import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil

object TestSumXYZ {

  private def readDicom(file: File) = {
    val al = new AttributeList
    al.read(file)
    al
  }

  private def makeVolume(fileList: Seq[File]): DicomVolume = {
    val alList = DicomVolume.sortDicomListByZ(fileList.map(file => readDicom(file)))
    new DicomVolume(alList.map(al => new DicomImage(al)))
  }

  def main(args: Array[String]): Unit = {
    println("Starting ...")
    val inDir = new File("""src\test\resources\TestDicomVolume""")
    val outDir = new File("""target\TestSumXYZ""")
    FileUtil.deleteFileTree(outDir)
    outDir.mkdirs

    val volume = makeVolume(inDir.listFiles)
    println("Read files, created volume.")

    val x = volume.sumX.toDeepColorBufferedImage(0)
    val y = volume.sumY.toDeepColorBufferedImage(0)
    val z = volume.sumZ.toDeepColorBufferedImage(0)

    ImageUtil.writePngFile(x, new File(outDir, "x.png"))
    ImageUtil.writePngFile(y, new File(outDir, "y.png"))
    ImageUtil.writePngFile(z, new File(outDir, "z.png"))

    println("Wrote files to " + outDir.getAbsolutePath)
  }
}