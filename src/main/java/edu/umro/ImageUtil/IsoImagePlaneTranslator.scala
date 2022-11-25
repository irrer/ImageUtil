/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.umro.ImageUtil

import com.pixelmed.dicom.{AttributeList, AttributeTag, TagFromName}
import edu.umro.DicomDict.TagByName

import java.awt.geom.Point2D

/**
  * Support mapping points between the isoplane and the image plane.  Isoplane values are in mm with
  * the origin at the center of the image.   Pixel plane values are in pixels with the origin in the
  * upper left corner of the image.  Functions do not factor in XRayImageReceptorTranslation, which if
  * needed, should be done by the caller.
  */

class IsoImagePlaneTranslator(al: AttributeList) {
  private def dblOf(tag: AttributeTag): Double = al.get(tag).getDoubleValues.head
  private def intOf(tag: AttributeTag): Int = al.get(tag).getIntegerValues.head

  //private val ImagePlanePixelSpacing = Phase2Util.getImagePlanePixelSpacing(al)
  val width: Int = intOf(TagFromName.Columns)
  val height: Int = intOf(TagFromName.Rows)

  private val ImagePlanePixelSpacing = al.get(TagByName.ImagePlanePixelSpacing).getDoubleValues

  val pixelSizeX: Double = ImagePlanePixelSpacing(0)
  val pixelSizeY: Double = ImagePlanePixelSpacing(1)

  val beamExpansionRatio: Double = dblOf(TagByName.RTImageSID) / dblOf(TagByName.RadiationMachineSAD)

  // Multiply this value by a measurement in mm in the isoplane to get the corresponding value in pixels in the image plane.
  private val expansionFactorX = beamExpansionRatio / pixelSizeX
  private val expansionFactorY = beamExpansionRatio / pixelSizeY

  // Upper left corner of image in isoplane coordinates
  private val imageUpperLeft: Point2D.Double = {
    val RTImagePosition = al.get(TagByName.RTImagePosition)
    if (RTImagePosition == null) {
      val x = -(((width - 1.0) / 2) * pixelSizeX) / beamExpansionRatio
      val y = -(((height - 1.0) / 2) * pixelSizeY) / beamExpansionRatio
      new Point2D.Double(x, y)
    } else {
      val x = RTImagePosition.getDoubleValues()(0) / beamExpansionRatio
      val y = (-RTImagePosition.getDoubleValues()(1)) / beamExpansionRatio
      new Point2D.Double(x, y)
    }
  }

  /**
    * Center of image in isoplane coordinates in mm.  Note that DICOM
    * standard gantry coordinates have the the sign of the Y axis negated,
    * so that positive is up in the image.
    */

  val caxCenter_iso: Point2D.Double = {
    val at = al.get(TagByName.XRayImageReceptorTranslation)
    if (at == null)
      new Point2D.Double(0, 0) // some devices do not support this, in which case the center of the image is assumed.
    else {
      val xy = at.getDoubleValues
      new Point2D.Double(-xy(0) / beamExpansionRatio, xy(1) / beamExpansionRatio)
    }
  }

  /**
    * Location in pixels of the CAX (center of rotation).  In other words, the
    * point in pixels at which the CAX should be drawn.
    */
  val caxCenter_pix: Point2D.Double = iso2Pix(caxCenter_iso)

  /**
    * Location of the CAX (center of rotation) as iso coordinates.
    */
  //  val caxCenter_isoX = pix2Iso(caxCenter_pix)

  /** convert x distance in mm from isoplane to pixel plane. */
  def iso2PixDistX(x: Double): Double = x * expansionFactorX

  /** convert y distance in mm from isoplane to pixel plane. */
  def iso2PixDistY(y: Double): Double = y * expansionFactorY

  /** convert x coordinate from isoplane in mm to pixel plane. */
  def iso2PixCoordX(x: Double): Double = (x - imageUpperLeft.getX) * expansionFactorX

  /** convert y coordinate from isoplane in mm to pixel plane. */
  def iso2PixCoordY(y: Double): Double = (y - imageUpperLeft.getY) * expansionFactorY

  /** convert a coordinate pair from isoplane in mm to pixel plane. */
  def iso2Pix(x: Double, y: Double): Point2D.Double = new Point2D.Double(iso2PixCoordX(x), iso2PixCoordY(y))

  /** convert a coordinate from isoplane in mm to pixel plane. */
  def iso2Pix(isoPoint: Point2D.Double): Point2D.Double = iso2Pix(isoPoint.getX, isoPoint.getY)

  /** convert x distance from pixel plane to isoplane in mm. */
  def pix2IsoDistX(x: Double): Double = x / expansionFactorX

  /** convert y distance from pixel plane to isoplane in mm. */
  def pix2IsoDistY(y: Double): Double = y / expansionFactorY

  /** convert x coordinate from pixel plane to isoplane in mm. */
  def pix2IsoCoordX(x: Double): Double = (x / expansionFactorX) + imageUpperLeft.getX

  /** convert y coordinate from pixel plane to isoplane in mm. */
  def pix2IsoCoordY(y: Double): Double = (y / expansionFactorY) + imageUpperLeft.getY

  /** convert a coordinate pair from pixel plane to isoplane in mm. */
  def pix2Iso(x: Double, y: Double): Point2D.Double = new Point2D.Double(pix2IsoCoordX(x), pix2IsoCoordY(y))

  /** convert a coordinate from pixel plane to isoplane in mm. */
  def pix2Iso(pixPoint: Point2D.Double): Point2D.Double = pix2Iso(pixPoint.getX, pixPoint.getY)

  /** Minimum point in the isoplane that is still in the image plane (can appear on the imager). */
  val minImage_mm: Point2D.Double = pix2Iso(0, 0)

  /** Maximum point in the isoplane that is still in the image plane (can appear on the imager). */
  val maxImage_mm: Point2D.Double = pix2Iso(width - 1, height - 1)

  def equalTo(other: IsoImagePlaneTranslator): Boolean = {
    (height == other.height) &&
    (width == other.width) &&
    (beamExpansionRatio == other.beamExpansionRatio) &&
    (pixelSizeX == other.pixelSizeX) &&
    (pixelSizeY == other.pixelSizeY)
  }

  override def toString: String = {
    "SID/SAD" + beamExpansionRatio.formatted("%12.8f") +
      "    SID: " + dblOf(TagByName.RTImageSID).formatted("%7.5f") + "    SAD: " + dblOf(TagByName.RadiationMachineSAD).formatted("%7.5f") +
      "    height: " + height + ", " + "    width: " + width + ", " +
      "    ImagePlanePixelSpacing: " + pixelSizeX.formatted("%7.5f") + ", " + pixelSizeY.formatted("%7.5f")
  }

  val validRTImagePosition: Boolean = {
    val RTImagePosition = al.get(TagByName.RTImagePosition)
    if (RTImagePosition == null)
      true
    else {
      val min = 0.00000001
      val xIso = RTImagePosition.getDoubleValues()(0)
      val yIso = -RTImagePosition.getDoubleValues()(1)
      val pix = iso2Pix(xIso, yIso)
      val valid = (pix.getX.abs < min) && (pix.getY.abs < min)
      valid
    }
  }

  // if (!validRTImagePosition) throw new RuntimeException("RTImagePosition is inconsistent with expected values"

}
