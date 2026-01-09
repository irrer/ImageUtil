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

import java.awt.image.BufferedImage
import scala.collection.mutable.ArrayBuffer

/**
  * Construct a watermark that can mark other images.  If the part of the original image to be watermarked is dark, then the watermark will
  * be lighter, otherwise darker.  Watermarking uses antialiasing, which uses the brightness (red+green+blue) of pixels to determine their
  * effect on the target image.
  *
  * It is assumed that the watermark is provided as dark symbols (letters, numbers) on a light background.  The
  * background in general does not affect the original image.
  *
  * The area of the image to be watermarked is checked for smoothness/noise.  If there is a lot of variation in
  * the original pixel values, then the contrast is increased by as much as 2 (maxContrastIncreaseFactor) to make
  * the watermark more visible.
  *
  * @param watermarkImage: Image used as the watermark.
  *
  * @param top True if watermarks are to be placed on the top edge of the final image.  If false, then bottom.
  *
  * @param left True if watermarks are to be placed on the left edge of the final image.  If false, then right.
  *
  * @param percentWidth Watermark should occupy this percent of the final image's width.
  *
  * @param percentChange Pixels should be changed by this amount.  A larger value (like 40) would produce more contrast.
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
  private case class Pix(x: Int, y: Int, brightness: Double)

  private val levelList = { for (x <- 0 until watermarkImage.getWidth; y <- 0 until watermarkImage.getHeight) yield brightnessOfWatermark(x, y) }.sorted

  // establish the range of brightness of the watermark
  private val sampleSize = 10
  private val watermarkDark = levelList.take(sampleSize).sum / sampleSize.toDouble
  private val watermarkLight = levelList.takeRight(sampleSize).sum / sampleSize.toDouble

  private val watermarkRange = watermarkLight - watermarkDark

  private case class Mask(width: Int, height: Int) {

    def this(image: BufferedImage) = this(image.getWidth, image.getHeight)

    private val ratio: Double = (width * percentWidth) / 100
    private val width_pix: Double = ratio.floor
    private val height_pix: Double = ((ratio * watermarkImage.getHeight) / watermarkImage.getWidth).floor

    private val xMin: Int = if (left) 0 else (width - width_pix).floor.toInt
    private val yMin: Int = if (top) 0 else (height - height_pix).floor.toInt
    private val xMax: Int = xMin + width_pix.toInt - 1
    private val yMax: Int = yMin + height_pix.toInt - 1

    private val pixList: IndexedSeq[Pix] = {

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

      def relevant(x: Int, y: Int): Boolean = {
        (x >= 0) && ((x + xMin) < width) &&
        (y >= 0) && ((y + yMin) < height) &&
        (x < scaledBuf.head.size) &&
        (y < scaledBuf.size) &&
        (scaledBuf(y)(x) > 0.01)
      }
      val pixList = for (y <- scaledBuf.indices; x <- scaledBuf.head.indices; if relevant(x, y)) yield { Pix(x + xMin, y + yMin, scaledBuf(y)(x)) }
      pixList
    }

    def mark(image: BufferedImage): Unit = {

      /**
        * If any color of the pixel is over half bright, then consider hte pixel bright.
        * @param x X coordinate
        * @param y Y coordinate
        * @return True if pixel is bright.
        */
      def isDark(x: Int, y: Int): Boolean = {
        val rgb = image.getRGB(x, y)
        val r = (rgb >> 16) & 0xff
        val g = (rgb >> 8) & 0xff
        val b = rgb & 0xff

        val is = (r < 128) && (g < 128) && (b < 128)
        is
      }

      // if the area to be watermarked is dark then make watermark brighter, or vice versa
      def changePix(p: Pix): Unit = {
        val v = image.getRGB(p.x, p.y)

        val v2: Int = {
          val chg = {
            val c = (255 * (percentChange / 100)).round.toInt
            if (isDark(p.x, p.y))
              c
            else
              -c
          }

          def bound(i: Int): Int = {
            0 match {
              case _ if i < 0   => 0
              case _ if i > 255 => 255
              case _            => i
            }
          }

          val rBefore = (v >> 16) & 0xff
          val gBefore = (v >> 8) & 0xff
          val bBefore = v & 0xff

          val rAfter = bound(rBefore + chg)
          val gAfter = bound(gBefore + chg)
          val bAfter = bound(bBefore + chg)

          val c = (rAfter << 16) + (gAfter << 8) + bAfter
          c
        }

        image.setRGB(p.x, p.y, v2)
      }
      pixList.foreach(p => changePix(p))

    }

  }

  private val cache = ArrayBuffer[Mask]()

  /**
    * Try to get a previously created mask.  If it does not exist, then make one and save it.
    *
    * Caching really helps performance because incoming images usually are of a small number of sizes.
    */
  private def getMask(width: Int, height: Int): Mask =
    cache.synchronized {
      cache.find(m => (m.width == width) && (m.height == height)) match {
        case Some(mask) => mask
        case _ =>
          val mask = Mask(width, height)
          cache += mask
          mask
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
