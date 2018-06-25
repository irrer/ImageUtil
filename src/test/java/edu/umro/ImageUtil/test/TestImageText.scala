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

class TestImageText extends FlatSpec with Matchers {

  "text pixels" should "not obscure source" in {
    TestImageText.perform
  }
}

object TestImageText {

  private def showBufImg(bufImg: BufferedImage) = {
    val frame = new JFrame
    frame.getContentPane.setLayout(new FlowLayout);
    frame.getContentPane.add(new JLabel(new ImageIcon(bufImg)))
    frame.pack
    frame.setVisible(true)
    Thread.sleep(5000)
  }

  private def perform = {
    val imgSize = 800
    val cntr = imgSize / 2
    val bufImg = new BufferedImage(imgSize, imgSize, BufferedImage.TYPE_INT_RGB)

    val graphics = ImageUtil.getGraphics(bufImg)

    val rh = new RenderingHints(
      RenderingHints.KEY_TEXT_ANTIALIASING,
      RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    graphics.setRenderingHints(rh);

    //ImageText.drawTextCenteredAt(graphics, cntr, cntr, "Centered Text")
    bufImg.setRGB(cntr, cntr, 0x00ffff)

    (0 to 359 by 45).map(d => ImageText.drawTextOffsetFrom(graphics, cntr, cntr, d.toString, d))
    //    ImageText.drawTextOffsetFrom(graphics, cntr, cntr, "90", 90)
    //    ImageText.drawTextOffsetFrom(graphics, cntr, cntr, "135", 135)
    //    ImageText.drawTextOffsetFrom(graphics, cntr, cntr, "180", 180)

    showBufImg(bufImg)
  }

  def main(args: Array[String]): Unit = {
    println("starting")

    //    val s90 = Math.sin((270 + 90).toRadians)
    //    val s30 = Math.sin((270 + 30).toRadians)
    //    val s390 = Math.sin((270 + 390).toRadians)
    //
    //    val c90 = Math.cos((270 + 90).toRadians)
    //    val c30 = Math.cos((270 + 30).toRadians)
    //    val c390 = Math.cos((270 + 390).toRadians)

    perform
    println("done")
    System.exit(0)
  }

}