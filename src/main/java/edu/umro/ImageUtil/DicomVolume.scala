
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

  /**
   * Get a sub-volume of the given volume given the min and max XYZ coordinates.
   */
  def getSubVolume(start: Point3i, size: Point3i): DicomVolume = {

    val rect = new Rectangle(start.getX, start.getY, size.getX, size.getY)

    val imgList = volume.drop(start.getZ).take(size.getZ).toList.map(img => img.getSubimage(rect))
    new DicomVolume(imgList)
  }

  /**
   * Get the point of the volume that is the highest pixel intensity.  Result is in units of voxels.
   */
  def getMaxPoint(volOfInt: DicomVolume): Point3d = {

    val xPlaneProfile = volOfInt.volume.map(img => img.rowSums).map(voxel => voxel.sum)
    val yPlaneProfile = volOfInt.volume.map(img => img.columnSums).map(voxel => voxel.sum)
    val zPlaneProfile = volOfInt.volume.map(img => img.rowSums.sum)

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

  def constructDicomVolume(alList: Seq[AttributeList]) = new DicomVolume(DicomVolume.sortDicomListByZ(alList.toSeq).map(al => new DicomImage(al)))
}