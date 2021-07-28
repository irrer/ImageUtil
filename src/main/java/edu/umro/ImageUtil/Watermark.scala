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

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Rectangle
import java.awt.Point
import edu.umro.ScalaUtil.Trace
import scala.collection.mutable.ArrayBuffer

/**
 * Construct a watermark that can mark other images.  If the part of the original image to be watermarked is dark, then the watermark will
 * be lighter, otherwise darker.  Watermarking uses antialiasing, which uses the brightness (red+green+blue) of pixels to determine their
 * effect on the target image.
 *
 * It is assumed that the watermark is provided as dark symbols (letters, numbers) on a light background.  The
 * background in general does not effect the original image.
 *
 * The area of the image to be watermarked is checked for smoothness/noise.  If there is a lot of variation in
 * the original pixel values, then the contrast is increased by as much as 2 (maxContrastIncreaseFactor) to make
 * the watermark more visible.
 *
 * @param image: Image used as the watermark.
 *
 * @param top: True if watermarks are to be placed on the top edge of the final image.  If false, then bottom.
 *
 * @param left: True if watermarks are to be placed on the left edge of the final image.  If false, then right.
 *
 * @param percentWidth: Watermark should occupy this percent of the final image's width.
 *
 * @param percentChange: Pixels should be changed by this amount.  A larger value (like 40) would produce more contrast.
 *
 */
class Watermark(watermarkImage: BufferedImage, top: Boolean, left: Boolean, percentWidth: Double, percentChange: Double) {

  private def brightnessOfWatermark(x: Int, y: Int) = {
    val rgb = watermarkImage.getRGB(x, y)
    (256 * 3) - (((rgb >> 16) & 0xff) +
      ((rgb >> 8) & 0xff) +
      (rgb & 0xff))
  }

  /** x and y are coordinates, brightness ranges from 0 to 1. */
  private case class Pix(x: Int, y: Int, brightness: Double);

  /** For noisy images, the contrast is multiplied by as much as this factor to make the watermark visible. */
  private val maxContrastIncreaseFactor = 2

  private val levelList = { for (x <- 0 until watermarkImage.getWidth; y <- 0 until watermarkImage.getHeight) yield (brightnessOfWatermark(x, y)) } sorted

  // establish the range of brightness of the watermark
  private val sampleSize = 10
  private val watermarkDark = levelList.take(sampleSize).sum / sampleSize.toDouble
  private val watermarkLight = levelList.takeRight(sampleSize).sum / sampleSize.toDouble

  private val watermarkRange = watermarkLight - watermarkDark

  private case class Mask(width: Int, height: Int) {

    def this(image: BufferedImage) = this(image.getWidth, image.getHeight)

    val ratio = (width * percentWidth) / 100
    val width_pix = ratio.floor
    val height_pix = ((ratio * watermarkImage.getHeight) / watermarkImage.getWidth).floor

    val xMin = if (left) 0 else (width - width_pix).floor.toInt
    val yMin = if (top) 0 else (height - height_pix).floor.toInt
    val xMax = (xMin + width_pix.toInt - 1)
    val yMax = (yMin + height_pix.toInt - 1)

    val numPixels = (xMax - xMin + 1) * (yMax - yMin + 1)

    val pixList: IndexedSeq[Pix] = {

      val buf = {
        def zero = ArrayBuffer.fill[Double](width_pix.toInt + 1)(0)
        Seq.fill(height_pix.toInt + 1)(zero)
      }

      // watermark to image
      def accumulate(x: Int, y: Int): Unit = {
        val brightness = (brightnessOfWatermark(x, y) - watermarkDark) / watermarkRange
        val xi = (x * (width_pix / watermarkImage.getWidth)).round.toInt
        val yi = (y * (height_pix / watermarkImage.getHeight)).round.toInt
        buf(yi)(xi) += brightness
      }

      for (x <- 0 until watermarkImage.getWidth; y <- 0 until watermarkImage.getHeight) yield { accumulate(x, y) }

      val scaledBuf = {
        val loMask = buf.flatten.min
        val hiMask = buf.flatten.max
        val range = hiMask - loMask

        buf.map(row => row.map(x => (x - loMask) / range))
      }

      def relevent(x: Int, y: Int): Boolean = {
        (x >= 0) && ((x + xMin) < width) &&
          (y >= 0) && ((y + yMin) < height) &&
          (x < scaledBuf.head.size) &&
          (y < scaledBuf.size) &&
          (scaledBuf(y)(x) > 0.01)
      }
      val pixList = for (y <- 0 until scaledBuf.size; x <- 0 until scaledBuf.head.size; if relevent(x, y)) yield { new Pix(x + xMin, y + yMin, scaledBuf(y)(x)) }
      pixList
    }

    def mark(image: BufferedImage): Unit = {

      def brightnessOfImage(x: Int, y: Int): Double = {
        val rgb = image.getRGB(x, y)
        (((rgb >> 16) & 0xff) +
          ((rgb >> 8) & 0xff) +
          (rgb & 0xff))
      }

      val brightnessList = for (x <- xMin to xMax; y <- yMin to yMax) yield (brightnessOfImage(x, y))
      val avg = brightnessList.sum / brightnessList.size
      val isDark = avg < ((256 / 2) * 3)
      val pctChange: Double = {
        val stdDev = Math.sqrt(brightnessList.map(b => (b - avg) * (b - avg)).sum / brightnessList.size)
        stdDev match {
          case _ if (stdDev < 1) => percentChange
          case _ if (stdDev > maxContrastIncreaseFactor) => percentChange * maxContrastIncreaseFactor
          case _ => percentChange * stdDev
        }
      }

      // force pixels to change by at this this amount
      val minChange = 255 * (percentChange / 100)
      val minPixChange = if (isDark) minChange.round.toInt else -minChange.round.toInt

      // if the area to be watermarked is dark then make watermark brighter, or vice versa
      def changePix(p: Pix) = {
        val change = pctChange * p.brightness / 100
        val mult = (if (isDark) (1 + change) else (1 - change))
        val v = image.getRGB(p.x, p.y)

        val rBefore = (v >> 16) & 0xff
        val gBefore = (v >> 8) & 0xff
        val bBefore = v & 0xff

        val rAfter = Math.min((rBefore * mult).round.toInt, 255) & 0xff
        val gAfter = Math.min((gBefore * mult).round.toInt, 255) & 0xff
        val bAfter = Math.min((bBefore * mult).round.toInt, 255) & 0xff

        val v2 = {
          val before = rBefore + gBefore + bBefore
          val after = rAfter + gAfter + bAfter
          def change = ((before.toDouble - after) / after).abs
          if ((after == 0) || (change < minChange)) {
            (Math.max(0, Math.min(255, rAfter + minPixChange)) << 16) +
              (Math.max(0, Math.min(255, gAfter + minPixChange)) << 8) +
              (Math.max(0, Math.min(255, bAfter + minPixChange)))
          } else
            (rAfter << 16) + (gAfter << 8) + bAfter
        }
        image.setRGB(p.x, p.y, v2)
      }
      pixList.map(p => changePix(p))

    }

  }

  private val cache = ArrayBuffer[Mask]()

  /**
   * Try to get a previously created mask.  If it does not exist, then make one and save it.
   *
   * Caching really helps performance because incoming images usually are of a small number of sizes.
   */
  private def getMask(width: Int, height: Int): Mask = cache.synchronized {
    cache.find(m => (m.width == width) && (m.height == height)) match {
      case Some(mask) => mask
      case _ => {
        val mask = new Mask(width, height)
        cache += mask
        mask
      }
    }
  }

  /**
   * Watermark the given image.
   */
  def mark(image: BufferedImage): Unit = {
    val mask = getMask(image.getWidth, image.getHeight)
    mask.mark(image)
  }

}


