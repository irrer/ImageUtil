package edu.umro.ImageUtil.test;

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import java.awt.image.BufferedImage
import java.awt.Color
import java.io.File
import javax.imageio.ImageIO
import scala.Left
import scala.Right
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.LocateEdge

//import scala.collection.JavaConverters._

object TestLocateEdge {

  def main(args: Array[String]): Unit = {

    val count = 10
    val range = (0 until count)

    val lo = 1.0.toFloat
    val hi = 9.0.toFloat

    def ramp(x: Int) = (((((hi - lo)) / count) * x) + lo).toFloat // linearly map index to value
    val data = range.map(i => lo) ++ range.map(i => ramp(i)) ++ range.map(i => hi)

    val midVal = LocateEdge.locateEdge(data, count / 4)

    println("data.size: " + data.size)
    println("midVal: " + midVal)
  }

}