
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
   * Get the point of the volume that is the highest pixel intensity.  Result is in units of voxels.
   */
  def getMaxPoint: Point3d = {

    val xPlaneProfile = volume.map(img => img.rowSums).map(voxel => voxel.sum)
    val yPlaneProfile = volume.map(img => img.columnSums).map(voxel => voxel.sum)
    val zPlaneProfile = volume.map(img => img.rowSums.sum)

    Trace.trace(xPlaneProfile)
    Trace.trace(yPlaneProfile)
    Trace.trace(zPlaneProfile)

    val xMax = xPlaneProfile.indexOf(xPlaneProfile.max)
    val yMax = yPlaneProfile.indexOf(yPlaneProfile.max)
    val zMax = zPlaneProfile.indexOf(zPlaneProfile.max)

    val xPosn = LocateMax.locateMax(xPlaneProfile)
    val yPosn = LocateMax.locateMax(yPlaneProfile)
    val zPosn = LocateMax.locateMax(zPlaneProfile)

    new Point3d(xPosn, yPosn, zPosn)
  }

}

object DicomVolume {

  private def slicePosition(attributeList: AttributeList): Double = attributeList.get(TagFromName.ImagePositionPatient).getDoubleValues()(2)

  /**
   * Sort a list of DICOM images by their Z position.
   */
  def sortDicomListByZ(alList: Seq[AttributeList]): Seq[AttributeList] = {
    alList.sortBy(al => slicePosition(al))
  }

  /**
   * construct a volume from a list of attribute lists.
   */
  def constructDicomVolume(alList: Seq[AttributeList]) = new DicomVolume(DicomVolume.sortDicomListByZ(alList.toSeq).map(al => new DicomImage(al)))

  /**
   * Convert a Point3i to Point3d
   */
  def pi2pd(pi: Point3i) = new Point3d(pi.getX, pi.getY, pi.getZ)
}