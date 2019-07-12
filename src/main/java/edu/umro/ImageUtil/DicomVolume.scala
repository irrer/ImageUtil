
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
    if (volume(z).get(x, y) == 0) // TODO rm
      System.currentTimeMillis
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
    //    val j = vList.map(v => sum(v, size)) // TODO rm
    //    println("\n" + vList.map(v => sum(v, size).toInt + " " + v).mkString("\n")) // TODO rm
    max
  }

  /**
   * Get the point of the volume that is the highest pixel intensity.  Result is in units of voxels.
   */
  def getMaxPoint: Point3d = {
    Trace.trace
    val xPlaneProfile = (0 until xSize).map(x => {
      val k = for (y <- 0 until ySize; z <- 0 until zSize) yield (getXYZ(x, y, z))
      k.sum
    })

    Trace.trace
    val rowSums = volume.map(img => img.rowSums)
    Trace.trace

    //    val xPlaneProfile = {
    //      def allXinY(y: Int) = {
    //        (0 until zSize).map(z => rowSums(z)(y)).sum
    //      }
    //      val j = (0 until ySize).map(y => allXinY(y))
    //      j
    //    }

    val yPlaneProfile = {
      (0 until ySize).map(y => {
        val k = for (x <- 0 until xSize; z <- 0 until zSize) yield (getXYZ(x, y, z))
        k.sum
      })
    }
    Trace.trace

    //    val yPlaneProfile = {
    //      val colSums = volume.map(img => img.columnSums)
    //      def allYinX(x: Int) = {
    //        (0 until zSize).map(z => rowSums(z)(x)).sum
    //      }
    //      val j = (0 until xSize).map(x => allYinX(x))
    //      j
    //    }

    val zPlaneProfile = volume.map(img => img.rowSums.sum)
    Trace.trace

    Trace.trace("\n  " + xPlaneProfile.map(f => f.toInt).mkString(" "))
    Trace.trace("\n  " + yPlaneProfile.map(f => f.toInt).mkString(" "))
    Trace.trace("\n  " + zPlaneProfile.map(f => f.toInt).mkString(" "))

    val xMax = xPlaneProfile.indexOf(xPlaneProfile.max)
    val yMax = yPlaneProfile.indexOf(yPlaneProfile.max)
    val zMax = zPlaneProfile.indexOf(zPlaneProfile.max)
    Trace.trace

    val xPosn = LocateMax.locateMax(xPlaneProfile)
    val yPosn = LocateMax.locateMax(yPlaneProfile)
    val zPosn = LocateMax.locateMax(zPlaneProfile)
    Trace.trace

    new Point3d(xPosn, yPosn, zPosn)
  }

}

object DicomVolume {

  private def slicePosition(attributeList: AttributeList): Double = {
    val j = attributeList.get(TagFromName.ImagePositionPatient).getDoubleValues()(2) // TODO rm
    val s = attributeList.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString // TODO rm
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