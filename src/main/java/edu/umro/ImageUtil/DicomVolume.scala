
package edu.umro.ImageUtil

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.Trace
import javax.vecmath.Point3d
import javafx.geometry.Point3D
import javax.vecmath.Point3i
import java.awt.Rectangle

case class DicomVolume(volume: Seq[DicomImage]) {

  //def this(alList:  Seq[AttributeList]) = this(DicomVolume.sortDicomListByZ(alList).map(al => new DicomImage(al)))

  val xSize = volume.head.width
  val ySize = volume.head.height
  val zSize = volume.size

  val volSize = Seq(xSize, ySize, zSize)

  /**
   * Get a sub-volume of the given volume given the min and max XYZ coordinates.
   *
   * @param start The corner of the 3D rectangle closest to the origin.
   *
   * @param size The XYZ dimensions of the 3D rectangle.
   */
  def getSubVolume(start: Point3i, size: Point3i): DicomVolume = {

    val rect = new Rectangle(start.getX, start.getY, size.getX, size.getY)

    val imgList = volume.drop(start.getZ).take(size.getZ).toList.map(img => img.getSubimage(rect))
    new DicomVolume(imgList)
  }

  /**
   * Get the value of a voxel.
   */
  def getXYZ(x: Int, y: Int, z: Int) = {
    volume(z).get(x, y)
  }

  /**
   * Get the sum of a sub-volume.
   */
  private def sum(start: Point3i, size: Point3i): Float = {
    val sum = (for (
      x <- start.getX until (start.getX + size.getX);
      y <- start.getY until (start.getY + size.getY);
      z <- start.getZ until (start.getZ + size.getZ)
    ) yield { getXYZ(x, y, z) }).sum
    sum
  }

  /**
   * Find the sub-volume of the given size that has the highest sum of voxel values.  In
   * other words, look for bright blobs of a given size.
   *
   * @param size Size of sub-volume
   *
   * @return Start of sub-volume (point closest to origin)
   */
  def getHighest(size: Point3i): Point3i = {
    val vList = for (
      x <- 0 until xSize - size.getX;
      y <- 0 until ySize - size.getY;
      z <- 0 until zSize - size.getZ
    ) yield (new Point3i(x, y, z))

    val max = vList.maxBy(v => sum(v, size))
    max
  }

  /**
   * Get the point of the volume that is the highest pixel intensity.  Result is in units of voxels.
   *
   * If the standard deviation in any of the 3 dimensions is below the given minimum standard deviation, then
   * return None, indicating that the maximum point found was not sufficiently distinguishable from
   * noise to qualify as an object.
   */
  def getMaxPoint(minStdDev: Double): Option[Point3d] = {

    // the sum of all planes perpendicular to the X axis
    val xPlaneProfile = (0 until xSize).map(x => {
      val xView = for (y <- 0 until ySize; z <- 0 until zSize) yield (getXYZ(x, y, z))
      xView.sum
    })

    // the sum of all planes perpendicular to the Y axis
    val yPlaneProfile = {
      (0 until ySize).map(y => {
        val yView = for (x <- 0 until xSize; z <- 0 until zSize) yield (getXYZ(x, y, z))
        yView.sum
      })
    }

    // the sum of all planes perpendicular to the Z axis
    val zPlaneProfile = volume.map(img => img.rowSums.sum)

    /**
     * Find the max point and verify that it is sufficiently large (as
     * opposed to just background noise).
     */
    def locateAndValidate(profile: Seq[Float]): Option[Double] = {
      val max = LocateMax.locateMax(profile)
      val mean = profile.sum / profile.size
      val sd = ImageUtil.stdDev(profile)
      val stdDevOfMax = (max - mean).abs / sd
      if (stdDevOfMax < minStdDev)
        None
      else
        Some(max)
    }

    val posn = Seq(
      locateAndValidate(xPlaneProfile),
      locateAndValidate(yPlaneProfile),
      locateAndValidate(zPlaneProfile))

    val flat = posn.flatten
    if (flat.size == 3)
      Some(new Point3d(flat(0), flat(1), flat(2)))
    else
      None
  }

}

object DicomVolume {

  private def slicePosition(attributeList: AttributeList): Double = {
    attributeList.get(TagFromName.ImagePositionPatient).getDoubleValues()(2)
  }

  /**
   * Sort a list of DICOM images by their Z position.
   */
  def sortDicomListByZ(alList: Seq[AttributeList]): Seq[AttributeList] = {
    alList.sortBy(al => slicePosition(al))
  }

  /**
   * construct a volume from a list of attribute lists.
   */
  def constructDicomVolume(alList: Seq[AttributeList]) = {
    new DicomVolume(DicomVolume.sortDicomListByZ(alList.toSeq).map(al => new DicomImage(al)))
  }

  /**
   * Convert a Point3i to Point3d
   */
  def pi2pd(pi: Point3i) = new Point3d(pi.getX, pi.getY, pi.getZ)
}