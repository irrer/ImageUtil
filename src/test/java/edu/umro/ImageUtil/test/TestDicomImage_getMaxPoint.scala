package edu.umro.ImageUtil.test;

import com.pixelmed.dicom.AttributeList
import java.io.File
import edu.umro.ImageUtil.DicomImage
import java.awt.Rectangle
import com.pixelmed.dicom.TagFromName
import java.awt.geom.Point2D
import javax.imageio.ImageIO
import edu.umro.util.Utility
import edu.umro.ImageUtil.ImageUtil
import java.awt.Color

/**
 * Test the getMaxPoint method.
 */
object TestDicomImage_getMaxPoint {

  val inDir = new File("src\\test\\resources\\GetMaxPoint")
  val outDir = new File("""target\GetMaxPoint""")

  private def readToAl(file: File): AttributeList = {
    val al = new AttributeList
    al.read(file)
    al
  }

  def main(args: Array[String]): Unit = {

    val radius_mm = 6.0
    val minStdDev = 1.2
    val zoomScale = 21

    /**
     * Cut a square rectangle out of the middle of the image twice the size of the radius_mm
     */
    def cutRect(al: AttributeList): DicomImage = {
      val pixSpace = al.get(TagFromName.ImagePlanePixelSpacing).getDoubleValues
      val x_mm = pixSpace(0)
      val y_mm = pixSpace(1)
      val rows = al.get(TagFromName.Rows).getIntegerValues.head
      val cols = al.get(TagFromName.Columns).getIntegerValues.head

      val x = ((cols / 2) - (radius_mm / x_mm)).round.toInt
      val y = ((rows / 2) - (radius_mm / y_mm)).round.toInt
      val w = ((radius_mm * 2) / x_mm).round.toInt
      val h = ((radius_mm * 2) / y_mm).round.toInt

      val image = (new DicomImage(al)).getSubimage(new Rectangle(x, y, w, h))
      image
    }

    def showMax(file: File, image: DicomImage, max: Point2D.Double) = {
      println("Found max at: " + max)

      val pngFile = new File(outDir, file.getName.replaceAll(".dcm$", ".png"))
      //val bufImg = ImageUtil.magnify(image.toDeepColorBufferedImage(0), zoomScale)
      val colorMap = ImageUtil.rgbColorMap(Color.white)
      val bufImg = ImageUtil.magnify(image.toBufferedImage(Color.yellow), zoomScale)
      val graphics = ImageUtil.getGraphics(bufImg)
      graphics.setColor(Color.white)
      val x = ((max.getX + 0.5) * zoomScale).floor.toInt
      val y = ((max.getY + 0.5) * zoomScale).floor.toInt
      graphics.drawLine(0, y, bufImg.getWidth - 1, y)
      graphics.drawLine(x, 0, x, bufImg.getHeight - 1)

      ImageIO.write(bufImg, "png", pngFile)
      println("Wrote image to " + pngFile.getAbsolutePath)
      println
    }

    def oneDot(image: DicomImage): DicomImage = {
      def colorOf(x: Int, y: Int): Float = {
        //if ((x == (image.width / 3)) && (y == (image.height / 5))) 100 else 0
        if ((x == 5) && (y == 5)) 100000 else 10000
      }
      val pixels = (0 until image.width).map(y => (0 until image.height).map(x => colorOf(x, y)))
      new DicomImage(pixels)
    }

    def testImage(file: File) = {
      println("Processing file " + file.getAbsolutePath)
      val al = readToAl(file)
      val image = cutRect(al)
      // val image = oneDot(cutRect(al))
      image.getMaxRect(xSize, ySize)
      image.getMaxPoint(minStdDev) match {
        case Some(max) => showMax(file, image, max)
        case _ => println("Did not find a max point")
      }
    }

    //System.setProperty("log4j2.debug", "INFO")
    val alList = inDir.listFiles.map(f => readToAl(f))
    Utility.deleteFileTree(outDir)
    outDir.mkdirs

    inDir.listFiles.map(file => testImage(file))

    println("TestDicomImage_getMaxPoint done")

  }

}