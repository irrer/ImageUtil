package edu.umro.LSDTable

import java.text.SimpleDateFormat

object LSDUtil {
    val dateFormat = new SimpleDateFormat("_yyyy-MM-dd_HH-mm-ss-SSS")

   val future = new SimpleDateFormat("yyyyMMddHHMM").parse("210001011111")

   def prnt(msg: String) = println(msg)
}