package edu.umro.ImageUtil.test;

import edu.umro.ImageUtil.LocateEdge
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.awt.image.BufferedImage
import edu.umro.ImageUtil.ImageUtil
import javax.swing.JFrame
import java.awt.FlowLayout
import javax.swing.JLabel
import javax.swing.ImageIcon
import edu.umro.ImageUtil.ImageText
import java.awt.RenderingHints

class TestImageUtil extends FlatSpec with Matchers {

  "text grats 43, 66" should "make nice range" in {
    val grats = ImageUtil.graticule(43, 66, 8)
    println("grats 43, 66: " + grats.mkString(", "))
    (grats(0)) should be(45)
  }

  "text grats 66, 43" should "make nice range" in {
    val grats = ImageUtil.graticule(66, 43, 8)
    println("grats 43, 66: " + grats.mkString(", "))
    (grats(0)) should be(65)
  }

  "text grats -133, 133" should "make nice range" in {
    val grats = ImageUtil.graticule(-133, 133, 7)
    println("grats -133, 133: " + grats.mkString(", "))
    (grats(0)) should be(-100)
    (grats(4)) should be(100)
  }

  "text grats " should "make nice range" in {
    val grats = ImageUtil.graticule(-0.00003457, -0.00003087, 5)
    println("grats -.00003457, -.00003112: " + grats.mkString(", "))
    (grats.size) should be(4)
  }

}
