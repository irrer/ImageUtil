package edu.umro.ImageUtil

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.Trace

import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Font
import java.awt.Graphics2D
import java.awt.Rectangle
import java.awt.geom.Rectangle2D
import javax.vecmath.Point2d
import javax.vecmath.Point2i

/**
  * Draw a BEV (beam's eye view) on a given image.
  */
case class DrawBEV(rtplan: AttributeList, rtimage: AttributeList) extends Logging {

  private def getBeamOfRtimage(rtplan: AttributeList, rtimage: AttributeList): AttributeList = {
    val ReferencedBeamNumber = rtimage.get(TagByName.ReferencedBeamNumber).getIntegerValues.head

    // @formatter:off
    val beamAl =
      DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence).
        find(beam => beam.get(TagByName.BeamNumber).
          getIntegerValues.head == ReferencedBeamNumber)
    // @formatter:on

    beamAl.get
  }

  private val colAngle_deg = DicomUtil.findAllSingle(rtimage, TagByName.BeamLimitingDeviceAngle).head.getDoubleValues.head
  private val colAngle_rad = Math.toRadians(colAngle_deg * -1)

  private val sin = Math.sin(colAngle_rad)
  private val cos = Math.cos(colAngle_rad)

  /**
    * Sort out the jaw an MLC values to simplify access.  If the
    * @param beamAl Attribute list of beam of interest.
    */
  case class ControlPointSequence(beamAl: AttributeList, rtimage: AttributeList) {

    /**
      * The leaf boundaries (positions of sides of leaves) for the given beam.
      */
    def leafBoundaryList: Seq[Double] = {
      val list = DicomUtil.findAllSingle(beamAl, TagByName.LeafPositionBoundaries)
      if (list.isEmpty)
        Seq()
      else
        list.head.getDoubleValues.toSeq
    }

    //noinspection ScalaUnusedSymbol
    private case class PosSeq(posSeq: AttributeList) {
      private val devType: String = posSeq.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrNull.trim
      val posList: Seq[Double] = posSeq.get(TagByName.LeafJawPositions).getDoubleValues.toSeq // toSeq to make this immutable

      val isXMlc: Boolean = devType.equals("MLCX")
      val isXJaw: Boolean = devType.equals("ASYMX") || devType.equals("X")
      val isYJaw: Boolean = devType.equals("ASYMY") || devType.equals("Y")
    }

    private val rtimageGantryAngle = DicomUtil.findAllSingle(rtimage, TagByName.GantryAngle).head.getDoubleValues.head

    /**
      * Convert arbitrary angle in degrees to a number 360 < degrees >= 0
      */
    private def modulo360(degrees: Double): Double = {
      ((degrees % 360.0) + 360.0) % 360.0
    }

    private def angleDiff(a: Double, b: Double): Double = {
      val diff = modulo360(a - b).abs
      (if (diff <= 180) diff else diff - 360).abs
    }

    // use the ControlPointSequence that has a gantry angle closest to the gantry angle of the RTIMAGE
    private val ControlPointSequence = {
      val cpsList = DicomUtil.seqToAttr(beamAl, TagByName.ControlPointSequence)
      def absGantryAngleDiff(al: AttributeList): Double = {
        val angleAttrList = DicomUtil.findAllSingle(al, TagByName.GantryAngle)
        val angleList = angleAttrList.flatMap(attr => attr.getDoubleValues)
        val diff = angleList.map(angle => angleDiff(angle, rtimageGantryAngle).abs).min
        diff
      }
      def hasGantryAngle(al: AttributeList): Boolean = {
        val angleAttrList = DicomUtil.findAllSingle(al, TagByName.GantryAngle)
        val angleList = angleAttrList.flatMap(attr => attr.getDoubleValues)
        angleList.nonEmpty
      }
      val cps = cpsList.filter(hasGantryAngle).minBy(absGantryAngleDiff)
      cps
    }

    // use the first ControlPointSequence
    // private val ControlPointSequence = DicomUtil.seqToAttr(beamAl, TagByName.ControlPointSequence).head

    // use the last ControlPointSequence
    // private val ControlPointSequence = DicomUtil.seqToAttr(beamAl, TagByName.ControlPointSequence).last

    private val posSeq = DicomUtil.seqToAttr(ControlPointSequence, TagByName.BeamLimitingDevicePositionSequence).map(PosSeq)

    val x1Jaw: Option[Double] = posSeq.find(_.isXJaw).map(_.posList.head)
    val x2Jaw: Option[Double] = posSeq.find(_.isXJaw).map(_.posList(1))

    val y1Jaw: Option[Double] = posSeq.find(_.isYJaw).map(_.posList.head)
    val y2Jaw: Option[Double] = posSeq.find(_.isYJaw).map(_.posList(1))

    val mlcX1PosList: Seq[Double] = posSeq.find(_.isXMlc) match {
      case Some(ps) => ps.posList.take(ps.posList.size / 2)
      case _        => Seq()
    }

    def mlcX1Pos(index: Int): Option[Double] = if ((index >= 0) && (index < mlcX1PosList.size)) Some(mlcX1PosList(index)) else None

    val mlcX2PosList: Seq[Double] = posSeq.find(_.isXMlc) match {
      case Some(ps) => ps.posList.takeRight(ps.posList.size / 2)
      case _        => Seq()
    }

    def mlcX2Pos(index: Int): Option[Double] = if ((index >= 0) && (index < mlcX2PosList.size)) Some(mlcX2PosList(index)) else None

  }

  val controlPointSequence: ControlPointSequence = ControlPointSequence(beamAl = getBeamOfRtimage(rtplan, rtimage), rtimage)

  /**
    * Draw the outline for the RTPLAN jaw and collimator.
    *
    * @param initialImage Draw on top of this image.
    * @param scale Enlargement factor.
    * @param color Color of jaw and MLC outlines.
    * @param font Font to use for labels.
    * @return A new image with the plan drawn on it.
    */
  //noinspection ScalaWeakerAccess
  def draw(
      initialImage: BufferedImage,
      scale: Int = 1,
      color: Color = Color.yellow,
      font: Font = new Font(ImageText.DefaultFont, Font.PLAIN, ImageText.DefaultTextPointSize),
      fontColor: Color = Color.red
  ): BufferedImage = {

    val beamAl = getBeamOfRtimage(rtplan, rtimage)

    val cps = ControlPointSequence(beamAl, rtimage)

    Trace.trace("leafBoundaryList: " + cps.leafBoundaryList.mkString("  "))

    val image = ImageUtil.magnify(initialImage, scale)
    // val image = ImageUtil.deepCopy(initialImage)

    // val gc = ImageUtil.getGraphics(image)
    // get graphics context without anti-aliasing

    // gc.setColor(color)
    // gc.setFont(font)

    val trans = new IsoImagePlaneTranslator(rtimage)

    /** Use this as a point in the isoplane that is beyond the edge of the imager. */
    val isoInfinity: Double = {
      val point0 = trans.pix2Iso(0, 0)
      Math.max(point0.getX.abs * 2, point0.getY.abs * 2)
    }

    /**
      * Translate an isoplane point to an image plane point.
      *
      * The Y axis is negated (mirrored vertically) because of an error in IsoImagePlaneTranslator.
      *
      * Scaling of the image is also done.  Note that an offset must also be done ( + (scale * 2)) to
      * put lines in the middle of enlarged pixels.
      *
      * @param point Point in isoplane.
      * @return Point in image plane.
      */
    def transPoint(point: Point2d): Point2i = {
      val scaleOffset = (scale - 1) / 2
      val x = point.getX
      val y = -point.getY // negate Y because IsoTranslator has Y coordinate reversed.
      val xx = x * cos - y * sin
      val tx = (trans.iso2PixCoordX(xx) * scale + scaleOffset).round.toInt
      val yy = x * sin + y * cos
      val ty = (trans.iso2PixCoordY(yy) * scale + scaleOffset).round.toInt
      new Point2i(tx, ty)
    }

    /**
      * Draw a line on the image.
      *
      * @param x1 X starting point in isoplane.
      * @param y1 Y starting point in isoplane.
      * @param x2 X ending point in isoplane.
      * @param y2 Y ending point in isoplane.
      */
    def drawLine(gc: Graphics2D, x1: Double, y1: Double, x2: Double, y2: Double): Unit = {
      val p1 = transPoint(new Point2d(x1, y1))
      val p2 = transPoint(new Point2d(x2, y2))
      gc.drawLine(p1.getX, p1.getY, p2.getX, p2.getY)
    }

    // make a scaled image as workspace and make it all black
    def makeWorkspace(): BufferedImage = {
      val img = ImageUtil.magnify(initialImage, scale)
      val gc = ImageUtil.getGraphics(img)
      gc.setColor(Color.black)
      gc.fillRect(0, 0, img.getWidth(), img.getWidth())
      img
    }

    /**
      * Fill the given rectangle with the color white.
      *
      * Coordinates passed as parameters are non-rotated in the isoplane.
      *
      * @param gc Draw in this graphics context.
      * @param xLo Low X coordinate.
      * @param yLo Low Y coordinate.
      * @param xHi High X coordinate.
      * @param yHi High Y coordinate.
      */
    def fillRect(gc: Graphics2D, xLo: Double, yLo: Double, xHi: Double, yHi: Double): Unit = {

      val yPix = trans.pixelSizeY

      val pointList = Seq(
        transPoint(new Point2d(xLo, yLo)),
        transPoint(new Point2d(xHi, yLo)),
        transPoint(new Point2d(xHi, yHi + yPix)),
        transPoint(new Point2d(xLo, yHi + yPix))
      )

      gc.fillPolygon(pointList.map(_.getX).toArray, pointList.map(_.getY).toArray, 4)
    }

    /**
      * Draw a single X1 MLC leaf and its end.
      *
      * @param index Index of leaf.
      */
    def drawX1Leaf(gc: Graphics2D, index: Int): Unit = {
      if (cps.mlcX1Pos(index).isDefined) {
        val xLo = -isoInfinity // off the left edge of the image plane
        val xHi = cps.mlcX1Pos(index).get
        val yLo = cps.leafBoundaryList(index)
        val yHi = cps.leafBoundaryList(index + 1)

        fillRect(gc, xLo, yLo, xHi, yHi)
      }
    }

    /**
      * Draw a single X1 MLC leaf and its end.
      *
      * @param index Index of leaf.
      */
    def drawX2Leaf(gc: Graphics2D, index: Int): Unit = {
      if (cps.mlcX2Pos(index).isDefined) {
        val xLo = cps.mlcX2Pos(index).get
        val xHi = isoInfinity // off the right edge of the image plane
        val yLo = cps.leafBoundaryList(index)
        val yHi = cps.leafBoundaryList(index + 1)

        fillRect(gc, xLo, yLo, xHi, yHi)
      }
    }

    def drawX1Jaw(gc: Graphics2D): Unit = {
      if (cps.x1Jaw.isDefined) {
        val xLo = -isoInfinity
        val xHi = cps.x1Jaw.get
        val yLo = -isoInfinity
        val yHi = isoInfinity

        fillRect(gc, xLo, yLo, xHi, yHi)
      }
    }

    def drawX2Jaw(gc: Graphics2D): Unit = {
      if (cps.x2Jaw.isDefined) {
        val xLo = cps.x2Jaw.get
        val xHi = isoInfinity
        val yLo = -isoInfinity
        val yHi = isoInfinity

        fillRect(gc, xLo, yLo, xHi, yHi)
      }
    }

    def drawY1Jaw(gc: Graphics2D): Unit = {
      if (cps.y1Jaw.isDefined) {
        val xLo = -isoInfinity
        val xHi = isoInfinity
        val yLo = -isoInfinity
        val yHi = cps.y1Jaw.get

        fillRect(gc, xLo, yLo, xHi, yHi)
      }
    }

    def drawY2Jaw(gc: Graphics2D): Unit = {
      if (cps.y2Jaw.isDefined) {
        val xLo = -isoInfinity
        val xHi = isoInfinity
        val yLo = cps.y2Jaw.get
        val yHi = isoInfinity

        fillRect(gc, xLo, yLo, xHi, yHi)
      }
    }

    /**
      * Draw the profile of the MLC on the final image.
      *
      * Do this by finding white pixels that are adjacent to a black pixels.  For all
      * such pixels, color a pixel in the final image.
      *
      * @param tmpImage Contains all area covered my MLC or jaws as white, other pixels as black.
      */
    def drawMlcProfile(tmpImage: BufferedImage): Unit = {

      // @formatter:off
      val adjacent = Seq(
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        // ( 0,  0),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1),
      )
      // @formatter:on

      val rgbWhite = Color.white.getRGB
      val rgbBlack = Color.black.getRGB

      // return true if the pixel at the given point is white and next to at least one black pixel.
      def whiteNextToBlack(x: Int, y: Int): Boolean = {
        if (tmpImage.getRGB(x, y) == rgbWhite) {
          val adjList = adjacent.map(adj => (adj._1 + x, adj._2 + y)).filter(adj => (adj._1 >= 0) && (adj._1 < tmpImage.getWidth()) && (adj._2 >= 0) && (adj._2 < tmpImage.getHeight()))
          adjList.exists(adj => tmpImage.getRGB(adj._1, adj._2) == rgbBlack)
        } else
          false
      }

      val rgb = color.getRGB // caller requested this color

      for (x <- 0 until tmpImage.getWidth(); y <- 0 until tmpImage.getHeight()) {
        if (whiteNextToBlack(x, y))
          image.setRGB(x, y, rgb)
      }

    }

    def drawMlc(): Unit = {
      if (cps.mlcX1PosList.nonEmpty) {
        val tmpImage = makeWorkspace()
        val gc = tmpImage.getGraphics.asInstanceOf[Graphics2D]
        gc.setColor(Color.white)
        cps.leafBoundaryList.indices.foreach(index => drawX1Leaf(gc, index))
        cps.leafBoundaryList.indices.foreach(index => drawX2Leaf(gc, index))
        drawX1Jaw(gc)
        drawX2Jaw(gc)
        drawY1Jaw(gc)
        drawY2Jaw(gc)

        drawMlcProfile(tmpImage)
      }
    }

    /**
      * If only the X jaw is defined, the draw X1 and X2.
      */
    def drawXJaw(): Unit = {
      if (cps.x1Jaw.isDefined && cps.y1Jaw.isEmpty) {
        val gc = image.getGraphics.asInstanceOf[Graphics2D]
        gc.setColor(color)
        drawLine(gc, cps.x1Jaw.get, -isoInfinity, cps.x1Jaw.get, isoInfinity) // X1
        drawLine(gc, cps.x2Jaw.get, -isoInfinity, cps.x2Jaw.get, isoInfinity) // X2
      }
    }

    /**
      * If only the Y jaw is defined, the draw Y1 and Y2.
      */
    def drawYJaw(): Unit = {
      if (cps.x1Jaw.isEmpty && cps.y1Jaw.isDefined) {
        val gc = image.getGraphics.asInstanceOf[Graphics2D]
        gc.setColor(color)
        drawLine(gc, -isoInfinity, cps.y1Jaw.get, isoInfinity, cps.y1Jaw.get) // Y1
        drawLine(gc, -isoInfinity, cps.y2Jaw.get, isoInfinity, cps.y2Jaw.get) // Y2
      }
    }

    val textGc = {
      val gc = ImageUtil.getGraphics(image)
      gc.setFont(font)
      gc.setColor(fontColor)
      gc
    }

    val textDimensions = ImageText.getTextDimensions(textGc, "X")

    def textVisible(xy: Point2i, text: String): Boolean = {
      val rec: Rectangle2D = {
        val r = ImageText.getTextDimensions(textGc, text)
        new Rectangle((r.getX + xy.getX).toInt, (r.getY + xy.getY).toInt, r.getWidth.toInt, r.getHeight.toInt)
      }

      def xOk(x: Double) = (x >= 0) && (x < image.getWidth())
      def yOk(y: Double) = (y >= 0) && (y < image.getHeight())

      xOk(rec.getX) && xOk(rec.getX + rec.getWidth) &&
      yOk(rec.getY) && yOk(rec.getY + rec.getHeight)
    }

    def drawX1JawLabel(): Unit = {
      val x = cps.x1Jaw.get - textDimensions.getWidth
      val y = {
        if (cps.y1Jaw.isDefined) (cps.y1Jaw.get + cps.y2Jaw.get) / 2
        else 0.0
      }

      val text = "X1"

      val xy = {
        val inside = transPoint(new Point2d(x, y))

        if (textVisible(inside, text))
          inside
        else {
          transPoint(new Point2d(cps.x1Jaw.get + textDimensions.getWidth, y))
        }
      }
      ImageText.drawTextCenteredAt(textGc, xy.getX, xy.getY, text)
    }

    def drawX2JawLabel(): Unit = {
      val x = cps.x2Jaw.get + textDimensions.getWidth
      val y = {
        if (cps.y1Jaw.isDefined) (cps.y1Jaw.get + cps.y2Jaw.get) / 2
        else 0.0
      }
      val text = "X2"

      val xy = {
        val inside = transPoint(new Point2d(x, y))

        if (textVisible(inside, text))
          inside
        else {
          transPoint(new Point2d(cps.x2Jaw.get - textDimensions.getWidth, y))
        }
      }
      ImageText.drawTextCenteredAt(textGc, xy.getX, xy.getY, text)
    }

    def drawY1JawLabel(): Unit = {
      val x = {
        if (cps.x1Jaw.isDefined) (cps.x1Jaw.get + cps.x2Jaw.get) / 2
        else 0.0
      }
      val y = cps.y1Jaw.get - textDimensions.getHeight / 2.0
      val text = "Y1"

      val xy = {
        val inside = transPoint(new Point2d(x, y))
        if (textVisible(inside, text))
          inside
        else
          transPoint(new Point2d(x, cps.y1Jaw.get + textDimensions.getHeight / 2.0))
      }
      ImageText.drawTextCenteredAt(textGc, xy.getX, xy.getY, text)
    }

    def drawY2JawLabel(): Unit = {
      val x = {
        if (cps.x1Jaw.isDefined) (cps.x1Jaw.get + cps.x2Jaw.get) / 2
        else 0.0
      }
      val y = cps.y2Jaw.get + textDimensions.getHeight / 2.0
      val text = "Y2"

      val xy = {
        val inside = transPoint(new Point2d(x, y))
        if (textVisible(inside, text))
          inside
        else
          transPoint(new Point2d(x, cps.y2Jaw.get - textDimensions.getHeight / 2.0))
      }

      ImageText.drawTextCenteredAt(textGc, xy.getX, xy.getY, text)
    }

    /**
      * Draw a simple rectangle outlining both jaws.
      */
    def drawBothJaws(): Unit = {
      if (cps.x1Jaw.isDefined && cps.y1Jaw.isDefined) {
        val gc = image.getGraphics.asInstanceOf[Graphics2D]
        gc.setColor(color)

        drawLine(gc, cps.x1Jaw.get, cps.y2Jaw.get, cps.x2Jaw.get, cps.y2Jaw.get) // top
        drawLine(gc, cps.x1Jaw.get, cps.y1Jaw.get, cps.x2Jaw.get, cps.y1Jaw.get) // bottom
        drawLine(gc, cps.x1Jaw.get, cps.y2Jaw.get, cps.x1Jaw.get, cps.y1Jaw.get) // left
        drawLine(gc, cps.x2Jaw.get, cps.y1Jaw.get, cps.x2Jaw.get, cps.y2Jaw.get) // right
      }
    }

    drawMlc() // only if MLC is defined
    drawBothJaws() // only if both X and Y jaws are defined
    drawXJaw() // only if X jaws are defined
    drawYJaw() // only if Y jaws are defined

    drawX1JawLabel()
    drawX2JawLabel()
    drawY1JawLabel()
    drawY2JawLabel()
    image

  }
}
