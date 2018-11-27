package edu.umro.ImageUtil

import smile.interpolation.BicubicInterpolation
import java.awt.geom.Rectangle2D

/**
 * Precisely locate a horizontal or vertical ridge using bicubic interpolation.
 */
class BicubicRidge(array: IndexedSeq[IndexedSeq[Float]]) {
  private def i2d(max: Int) = (0 until max).map(x => x.toDouble).toArray

  private val bicubicInterpolation = new BicubicInterpolation(i2d(array.size), i2d(array.head.size), array.map(row => row.map(col => col.toDouble).toArray).toArray)

  def interpolate(x: Double, y: Double) = bicubicInterpolation.interpolate(y, x)

  private val searchDepth = 20

  /**
   * Find the x position of the vertical ridge with the given rectangular bounding box.
   */
  def Xvertical(rect: Rectangle2D.Double, increment: Double): Double = {
    val vertCount = (rect.height / increment).round.toInt
    val horzCount = (rect.width / increment).round.toInt

    val xLo = rect.x
    val xHi = rect.x + rect.width

    def highestAt(y: Double): Double = {
      val list = (0 until horzCount).map(xi => bicubicInterpolation.interpolate(y, xi * increment))
      val avg = list.sum / horzCount
      val aboveAvg = (0 until horzCount).filter(xi => list(xi) > avg)
      val midpoint = (aboveAvg.sum * increment) / aboveAvg.size
      midpoint
    }

    val ridgeList = (0 until vertCount).map(yi => highestAt(yi * increment))
    val avg = ridgeList.sum / ridgeList.size
    avg
  }

  /**
   * Find the x position of the vertical ridge with the given rectangular bounding box.
   */
  def vertical(rect: Rectangle2D.Double, increment: Double): Double = {
    val vertCount = (rect.height / increment).round.toInt
    val horzCount = (rect.width / increment).round.toInt

    val xLo = rect.x
    val xHi = rect.x + rect.width

    def highestAt(y: Double): Double = {
      val list = (0 until horzCount).map(xi => bicubicInterpolation.interpolate(y, xi * increment))
      val avg = list.sum / horzCount
      val aboveAvg = (0 until horzCount).filter(xi => list(xi) > avg)
      val midpoint = (aboveAvg.sum * increment) / aboveAvg.size
      midpoint
    }

    val ridgeList = (0 until vertCount).map(yi => highestAt(yi * increment))
    val avg = ridgeList.sum / ridgeList.size
    avg
  }

}