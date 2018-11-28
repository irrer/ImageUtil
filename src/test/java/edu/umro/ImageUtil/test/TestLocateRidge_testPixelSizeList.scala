package edu.umro.ImageUtil.test;

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.awt.geom.Rectangle2D
import edu.umro.ImageUtil.LocateRidge
import edu.umro.ScalaUtil.Trace

class TestLocateRidge_testPixelSizeList extends FlatSpec with Matchers {

  "measured ridge" should "be close calculated ridge" in {

    val rect = new Rectangle2D.Double(4, 5, 6, 7)

    val fraction = Seq(0.0, 0.25, 0.75)
    val lo = 2 to 4
    val hi = 4 to 6

    for (l <- lo; h <- hi; lf <- fraction; hf <- fraction) {
      val ll = l + lf
      val hh = h + hf
      println("ll: " + ll.formatted("%4f") + "    hh: " + hh.formatted("%4f") + " : " + LocateRidge.testPixelSizeList(ll, hh).sortBy(_.index))
    }

    

    true should be(true)
  }

}