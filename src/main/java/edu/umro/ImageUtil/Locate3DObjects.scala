
package edu.umro.ImageUtil

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import java.awt.image.BufferedImage
import edu.umro.ScalaUtil.Trace
import scala.collection.mutable.ArrayBuffer
import javax.vecmath.Point3d

class Locate3DObjects(alList: Seq[AttributeList]) {

  private def zPosn(al: AttributeList) = al.get(TagFromName.ImagePositionPatient).getDoubleValues()(2)

  /** List of images, sorted by position. */
  val imgList = alList.sortBy(al => zPosn(al)).map(al => new DicomImage(al))

  val width = imgList.head.width
  val height = imgList.head.height

  /**
   * Make a mutable list of all voxels that are part of a 3D object
   */
  private def findBrightVoxels(threshold: Int): ArrayBuffer[Point3d] = {
    val brightVoxels = ArrayBuffer[Point3d]()
    def voxelsOfImage(z: Int) = {
      val img = imgList(z)
      for (x <- (0 until width); y <- (0 until height)) {
        if (img.get(x, y) >= threshold)
          brightVoxels :+ new Point3d(x, y, z)
      }
    }
    imgList.indices.map(z => voxelsOfImage(z))
    brightVoxels

  }

  /**
   * Locate all 3D objects that are composed of clusters of voxels.
   *
   * @param threshold
   */
  def locate(threshold: Int): Seq[Seq[Point3d]] = {

    val brightVoxels = findBrightVoxels(threshold)

    val p = new Point3d(1, 3, 5)
    val j = p.getX
    ???
  }
}
