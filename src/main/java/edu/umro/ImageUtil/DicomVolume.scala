
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

  val volSize = new Point3i(xSize, ySize, zSize)

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
   * Get the sum of each line of voxels in the X axis.  In other words, all
   * voxels with the same y and z coordinates are summed.  This is akin to
   * creating a 2 dimensional profile seen from the X axis.
   */
  def sumX: DicomImage = {
    def getRow(yy: Int): IndexedSeq[Float] = {
      for (z <- 0 until zSize) yield { (0 until xSize).map(x => getXYZ(x, yy, z)).sum }
    }

    val matrix = for (y <- 0 until ySize) yield getRow(y)

    new DicomImage(matrix)
  }

  /**
   * Get the sum of each line of voxels in the Y axis.  In other words, all
   * voxels with the same x and z coordinates are summed.  This is akin to
   * creating a 2 dimensional profile seen from the Y axis.
   */
  def sumY: DicomImage = {
    def getRow(zz: Int): IndexedSeq[Float] = {
      for (x <- 0 until xSize) yield { (0 until ySize).map(y => getXYZ(x, y, zz)).sum }
    }

    val matrix = for (z <- 0 until zSize) yield getRow(z)

    new DicomImage(matrix)
  }

  /**
   * Get the sum of each line of voxels in the Z axis.  In other words, all
   * voxels with the same x and y coordinates are summed.  This is akin to
   * creating a 2 dimensional profile seen from the Z axis.
   */
  def sumZ: DicomImage = {
    def getRow(yy: Int): IndexedSeq[Float] = {
      for (x <- 0 until xSize) yield { (0 until zSize).map(z => getXYZ(x, yy, z)).sum }
    }

    val matrix = for (y <- 0 until ySize) yield getRow(y)

    new DicomImage(matrix)
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

  /** Get the sum of all planes perpendicular to the X axis */
  def xPlaneProfile = {
    val prof = (0 until xSize).map(x => {
      val xView = for (y <- 0 until ySize; z <- 0 until zSize) yield (getXYZ(x, y, z))
      xView.sum
    })
    new Profile(prof)
  }

  /** Get the sum of all planes perpendicular to the Y axis */
  def yPlaneProfile = {
    val prof = (0 until ySize).map(y => {
      val yView = for (x <- 0 until xSize; z <- 0 until zSize) yield (getXYZ(x, y, z))
      yView.sum
    })
    new Profile(prof)
  }

  /** Get the sum of all planes perpendicular to the Z axis */
  def zPlaneProfile = new Profile(volume.map(img => img.rowSums.sum))

  //  /**
  //   * Get the point of the volume that is the center of the highest pixel intensity.  This is done by getting
  //   * the profile in each of the three axis and finding the maximum point on the cubic spline curve.  Result
  //   * is in units of voxels.
  //   *
  //   * If the standard deviation in any of the 3 dimensions is below the given minimum standard deviation, then
  //   * return None, indicating that the maximum point found was not sufficiently distinguishable from
  //   * noise to qualify as an object.
  //   */
  //  def getMaxPoint(minStdDev: Double): Option[Point3d] = {
  //
  //    /**
  //     * Find the max point and verify that it is sufficiently large (as
  //     * opposed to just background noise).
  //     */
  //    def locateAndValidate(profile: Seq[Float]): Option[Double] = {
  //      val maxX = LocateMax.locateMax(profile)
  //      val spline = LocateMax.toCubicSpline(profile)
  //      val maxY = spline.evaluate(maxX)
  //
  //      val mean = profile.sum / profile.size
  //      val sd = ImageUtil.stdDev(profile)
  //      val stdDevOfMax = (maxY - mean).abs / sd
  //      if (stdDevOfMax < minStdDev)
  //        None
  //      else
  //        Some(maxX)
  //    }
  //
  //    val posn = Seq(
  //      locateAndValidate(xPlaneProfile),
  //      locateAndValidate(yPlaneProfile),
  //      locateAndValidate(zPlaneProfile))
  //
  //    val flat = posn.flatten
  //    if (flat.size == 3)
  //      Some(new Point3d(flat(0), flat(1), flat(2)))
  //    else
  //      None
  //  }

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