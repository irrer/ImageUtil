package edu.umro.LSDTable

import java.text.SimpleDateFormat

object LSDUtil {
  val dateFormat = new SimpleDateFormat("_yyyy-MM-dd_HH-mm-ss-SSS")

  val future = new SimpleDateFormat("yyyyMMddHHMM").parse("210001011111")

  def prnt(msg: String) = println(msg)

  /**
   * Convert arbitrary angle in degrees to a number nearest to zero.  e.g.: 359.5 --> -.5
   */
  def angleCloseToZero(degrees: Double): Double = {
    val d = ((degrees % 360.0) + 360.0) % 360.0
    if (d > 180) d - 360 else d
  }

}