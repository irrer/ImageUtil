package edu.umro.ImageUtil

import com.pixelmed.dicom.AttributeList
import java.awt.geom.Point2D
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName

/**
 * Support mapping points between the isoplane and the image plane.  Isoplane values are in mm with
 * the origin at the center of the image.   Pixel plane values are in pixels with the origin in the
 * upper left corner of the image.
 */

class IsoImagePlaneTranslator(al: AttributeList) {
  private def dblOf(tag: AttributeTag): Double = al.get(tag).getDoubleValues.head
  private def intOf(tag: AttributeTag): Int = al.get(tag).getIntegerValues.head

  //private val ImagePlanePixelSpacing = Phase2Util.getImagePlanePixelSpacing(al)
  val width = intOf(TagFromName.Columns)
  val height = intOf(TagFromName.Rows)

  private val ImagePlanePixelSpacing = al.get(TagFromName.ImagePlanePixelSpacing).getDoubleValues

  val pixelSizeX = ImagePlanePixelSpacing(0)
  val pixelSizeY = ImagePlanePixelSpacing(1)

  val beamExpansionRatio = dblOf(TagFromName.RTImageSID) / dblOf(TagFromName.RadiationMachineSAD)

  // Multiply this value by a measurement in mm in the isoplane to get the corresponding value in pixels in the image plane.
  private val expansionFactorX = beamExpansionRatio / pixelSizeX
  private val expansionFactorY = beamExpansionRatio / pixelSizeY

  // Upper left corner of image in isoplane coordinates
  private val imageUpperLeft: Point2D.Double = {
    val RTImagePosition = al.get(TagFromName.RTImagePosition)
    if (RTImagePosition == null) {
      val x = -(((width - 1.0) / 2) * pixelSizeX) / beamExpansionRatio
      val y = -(((height - 1.0) / 2) * pixelSizeY) / beamExpansionRatio
      new Point2D.Double(x, y)
    } else {
      val x = RTImagePosition.getDoubleValues()(0) / beamExpansionRatio
      val y = (-(RTImagePosition.getDoubleValues()(1))) / beamExpansionRatio
      new Point2D.Double(x, y)
    }
  }

  /** convert x distance in mm from isoplane to pixel plane. */
  def iso2PixDistX(x: Double) = x * expansionFactorX
  /** convert y distance in mm from isoplane to pixel plane. */
  def iso2PixDistY(y: Double) = y * expansionFactorY

  /** convert x coordinate from isoplane in mm to pixel plane. */
  def iso2PixCoordX(x: Double) = (x - imageUpperLeft.getX) * expansionFactorX
  /** convert y coordinate from isoplane in mm to pixel plane. */
  def iso2PixCoordY(y: Double) = (y - imageUpperLeft.getY) * expansionFactorY

  /** convert a coordinate pair from isoplane in mm to pixel plane. */
  def iso2Pix(x: Double, y: Double): Point2D.Double = new Point2D.Double(iso2PixCoordX(x), iso2PixCoordY(y))
  /** convert a coordinate from isoplane in mm to pixel plane. */
  def iso2Pix(isoPoint: Point2D.Double): Point2D.Double = iso2Pix(isoPoint.getX, isoPoint.getY)

  /** convert x distance from pixel plane to isoplane in mm. */
  def pix2IsoDistX(x: Double) = x / expansionFactorX
  /** convert y distance from pixel plane to isoplane in mm. */
  def pix2IsoDistY(y: Double) = y / expansionFactorY

  /** convert x coordinate from pixel plane to isoplane in mm. */
  def pix2IsoCoordX(x: Double) = (x / expansionFactorX) + imageUpperLeft.getX
  /** convert y coordinate from pixel plane to isoplane in mm. */
  def pix2IsoCoordY(y: Double) = (y / expansionFactorY) + imageUpperLeft.getY

  /** convert a coordinate pair from pixel plane to isoplane in mm. */
  def pix2Iso(x: Double, y: Double): Point2D.Double = new Point2D.Double(pix2IsoCoordX(x), pix2IsoCoordY(y))
  /** convert a coordinate from pixel plane to isoplane in mm. */
  def pix2Iso(isoPoint: Point2D.Double): Point2D.Double = pix2Iso(isoPoint.getX, isoPoint.getY)

  /** Minimum point in the isoplane that is still in the image plane (can appear on the imager). */
  val minImage_mm = pix2Iso(0, 0)
  /** Maximum point in the isoplane that is still in the image plane (can appear on the imager). */
  val maxImage_mm = pix2Iso(width - 1, height - 1)

  def equalTo(other: IsoImagePlaneTranslator) = {
    (height == other.height) &&
      (width == other.width) &&
      (beamExpansionRatio == other.beamExpansionRatio) &&
      (pixelSizeX == other.pixelSizeX) &&
      (pixelSizeY == other.pixelSizeY)
  }

  override def toString = {
    "SID/SAD" + beamExpansionRatio.formatted("%12.8f") +
      "    SID: " + dblOf(TagFromName.RTImageSID).formatted("%7.5f") + "    SAD: " + dblOf(TagFromName.RadiationMachineSAD).formatted("%7.5f") +
      "    height: " + height + ", " + "    width: " + width + ", " +
      "    ImagePlanePixelSpacing: " + pixelSizeX.formatted("%7.5f") + ", " + pixelSizeY.formatted("%7.5f")
  }

  val validRTImagePosition: Boolean = {
    val RTImagePosition = al.get(TagFromName.RTImagePosition)
    if (RTImagePosition == null)
      true
    else {
      val min = 0.00000001
      val xIso = RTImagePosition.getDoubleValues()(0)
      val yIso = -(RTImagePosition.getDoubleValues()(1))
      val pix = iso2Pix(xIso, yIso)
      val valid = (pix.getX.abs < min) && (pix.getY.abs < min)
      valid
    }
  }

  // if (!validRTImagePosition) throw new RuntimeException("RTImagePosition is inconsistent with expected values"

}

