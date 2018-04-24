package edu.umro.ImageUtil2.copy

import com.pixelmed.dicom.AttributeList
import io.scif.img.SCIFIOImgPlus
import com.pixelmed.dicom.TagFromName
import io.scif.img.ImgOpener
import java.awt.image.BufferedImage
import net.imglib2.view.Views
import net.imglib2.`type`.numeric.RealType
import scala.collection.JavaConverters._
import java.io.File
import javax.imageio.ImageIO
import net.imglib2.`type`.NativeType
import net.imglib2.`type`.numeric.real.FloatType
import net.imglib2.RandomAccessibleInterval
import net.imglib2.`type`.numeric.integer.ShortType

object AnalyzeBadPixels {

  type PixelType = NativeType[_] with RealType[_]
  type RT = RealType[_]

  type FT = FloatType
  type ST = ShortType

  /**
   * Given an image, determine which pixels are bad.
   */
  def analyzeBadPixels(imgPlus: SCIFIOImgPlus[net.imglib2.RandomAccessibleInterval[FloatType]], count: Int) = {

    if (true) {
      val j: SCIFIOImgPlus[FT] = null;

      FindWorstPixels.findWorstPixels(j, 5)
    }

    if (true) {
      val j: SCIFIOImgPlus[ST] = null;

      FindWorstPixels.findWorstPixels(j, 5)
    }

//    if (true) {
//      val j: RandomAccessibleInterval[PixelType] = null;
//
//      FindWorstPixels.findWorstPixels(j, 5)
//    }
//
//    if (true) {
//      val j: RandomAccessibleInterval[ST] = null;
//
//      FindWorstPixels.findWorstPixels(j, 5)
//    }
//
//    FindWorstPixels.findWorstPixels(imgPlus, 5)
//
//    if (true) {
//      val j: SCIFIOImgPlus[_] = null;
//
//      FindWorstPixels.findWorstPixels(j, 5)
//    }
//
//    if (true) {
//      val j: SCIFIOImgPlus[RT] = null;
//
//      FindWorstPixels.findWorstPixels(j, 5)
//    }

    if (true) {
      val j: RandomAccessibleInterval[FT] = null;

      FindWorstPixels.findWorstPixels(j, 5)
    }

    //    if (true) {
    //      val j: RandomAccessibleInterval[T <: RealType[T]] = null;
    //
    //      FindWorstPixels.findWorstPixels(j, 5)
    //    }
    //
    //    if (true) {
    //      val j: RandomAccessibleInterval[T >: RealType] = null;
    //
    //      FindWorstPixels.findWorstPixels(j, 5)
    //    }

  }

}