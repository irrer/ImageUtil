package edu.umro.ImageUtil

import java.awt.Graphics2D
import java.awt.geom.Point2D
import java.awt.image.BufferedImage
import java.awt.Color
import java.io.File
import javax.vecmath.Point2d

/**
  * Utilities for drawing on a scaled image.
  *
  * @param scale Image magnification factor.
  */
case class ScaledImage(scale: Int, offsetX: Int = 0, offsetY: Int = 0) {

  def round(d: Double): Int = d.round.toInt

  /**
    * Scale a pixel coordinate by the given scale to produce a scaled coordinate in pixel space.
    */
  private def scalePixel(pix: Double): Int = {
    round((scale * (pix + 0.5)) - 0.5)
  }

  //noinspection ScalaWeakerAccess
  def scalePixelX(pix: Double): Int = {
    scalePixel(pix - offsetX)
  }

  //noinspection ScalaWeakerAccess
  def scalePixelY(pix: Double): Int = {
    scalePixel(pix - offsetY)
  }

  def drawLine(gc: Graphics2D, x1: Double, y1: Double, x2: Double, y2: Double): Unit = {
    gc.drawLine(
      scalePixelX(x1),
      scalePixelY(y1),
      scalePixelX(x2),
      scalePixelY(y2)
    )
  }

  def drawLine(gc: Graphics2D, p1: Point2d, p2: Point2d): Unit = {
    drawLine(gc, p1.x, p1.y, p2.x, p2.y)
  }

  def drawLine(gc: Graphics2D, p1: Point2D.Double, p2: Point2d): Unit = {
    drawLine(gc, p1.x, p1.y, p2.x, p2.y)
  }

  def drawTextCenteredAt(graphics: Graphics2D, x: Double, y: Double, text: String): Unit = {
    ImageText.drawTextCenteredAt(graphics: Graphics2D, scalePixelX(x), scalePixelY(y), text: String)
  }

  //noinspection ScalaWeakerAccess
  def drawOval(gc: Graphics2D, x: Double, y: Double, width: Double, height: Double): Unit = {
    gc.drawOval( //
      scalePixelX(x),
      scalePixelY(y),
      round(width * scale),
      round(height * scale)
    )
  }

  //noinspection ScalaUnusedSymbol
  def drawOval(gc: Graphics2D, p: Point2d, width: Double, height: Double): Unit =
    drawOval(gc, p.getX, p.getY, width, height)

  def magnify(img: BufferedImage): BufferedImage = ImageUtil.magnify(img, scale)
}

/**
 * For testing only.
 */
object ScaledImage {
  def main(args: Array[String]): Unit = {
    val w = 10
    val h = 10
    val img = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)

    def clr(x: Int, y: Int): Int = {
      (x * 50) + (y * 50)
    }

    (0 until w).map(x => (0 until h).map(y => img.setRGB(x, y, clr(x, y))))

    val si = ScaledImage(11)

    val big = si.magnify(img)

    val gc = ImageUtil.getGraphics(big)
    gc.setColor(Color.orange)

    si.drawLine(gc, 3, 3, 5, 5)
    si.drawLine(gc, 0, 0, 0, 3)
    si.drawLine(gc, 0, 0, 5, 0)

    val file = new File("""D:/tmp/foo.png""")

    ImageUtil.writePngFile(big, file)
    println(s"Wrote file $file")
  }

}
