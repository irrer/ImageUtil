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

package edu.umro.ImageUtil.test

import com.pixelmed.dicom.AttributeList

import java.awt.geom.Point2D
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.IsoImagePlaneTranslator
//import org.aqa.webrun.phase2.Phase2Util

/**
 * Support mapping points between the isoplane and the image plane.  Isoplane values are in mm with
 * the origin at the center of the image.   Pixel plane values are in pixels with the origin in the
 * upper left corner of the image.
 */

class IsoImagePlaneTranslatorOld(al: AttributeList) {
  private def dblOf(tag: AttributeTag): Double = al.get(tag).getDoubleValues.head
  private def intOf(tag: AttributeTag): Int = al.get(tag).getIntegerValues.head

  //private val ImagePlanePixelSpacing = Phase2Util.getImagePlanePixelSpacing(al)
  val width = intOf(TagFromName.Columns)
  val height = intOf(TagFromName.Rows)

  // Image center in pixels
  val imageCenter = new Point2D.Double(((width - 1.0) / 2), ((height - 1.0) / 2))

  val beamExpansionRatio = dblOf(TagByName.RTImageSID) / dblOf(TagByName.RadiationMachineSAD)

  private val ImagePlanePixelSpacing = al.get(TagByName.ImagePlanePixelSpacing).getDoubleValues

  val pixelSizeX = ImagePlanePixelSpacing(0)
  val pixelSizeY = ImagePlanePixelSpacing(1)

  // Multiply this value by a measurement in mm in the isoplane to get the corresponding value in pixels in the image plane.
  private val expansionFactorX = beamExpansionRatio / pixelSizeX
  private val expansionFactorY = beamExpansionRatio / pixelSizeY

  /** convert x distance in mm from isoplane to pixel plane. */
  def iso2PixDistX(x: Double) = x * expansionFactorX
  /** convert y distance in mm from isoplane to pixel plane. */
  def iso2PixDistY(y: Double) = y * expansionFactorY

  /** convert x coordinate from isoplane in mm to pixel plane. */
  def iso2PixCoordX(x: Double) = (x * expansionFactorX) + imageCenter.getX
  /** convert y coordinate from isoplane in mm to pixel plane. */
  def iso2PixCoordY(y: Double) = (y * expansionFactorY) + imageCenter.getY

  /** convert a coordinate pair from isoplane in mm to pixel plane. */
  def iso2Pix(x: Double, y: Double): Point2D.Double = new Point2D.Double(iso2PixCoordX(x), iso2PixCoordY(y))
  /** convert a coordinate from isoplane in mm to pixel plane. */
  def iso2Pix(isoPoint: Point2D.Double): Point2D.Double = iso2Pix(isoPoint.getX, isoPoint.getY)

  /** convert x distance from pixel plane to isoplane in mm. */
  def pix2IsoDistX(x: Double) = x / expansionFactorX
  /** convert y distance from pixel plane to isoplane in mm. */
  def pix2IsoDistY(y: Double) = y / expansionFactorY

  /** convert x coordinate from pixel plane to isoplane in mm. */
  def pix2IsoCoordX(x: Double) = (x - imageCenter.getX) / expansionFactorX
  /** convert y coordinate from pixel plane to isoplane in mm. */
  def pix2IsoCoordY(y: Double) = (y - imageCenter.getY) / expansionFactorY

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
      "    SID: " + dblOf(TagByName.RTImageSID).formatted("%7.5f") + "    SAD: " + dblOf(TagByName.RadiationMachineSAD).formatted("%7.5f") +
      "    height: " + height + ", " + "    width: " + width + ", " +
      "    ImagePlanePixelSpacing: " + pixelSizeX.formatted("%7.5f") + ", " + pixelSizeY.formatted("%7.5f")
  }

}

