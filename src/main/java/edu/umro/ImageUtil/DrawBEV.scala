package edu.umro.ImageUtil

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.Trace

import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Font
import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
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
    * Get the leaf boundaries (positions of sides of leaves) for the given beam.
    *
    * @param beamAl Beam sequence for the beam of interest.
    * @return List of leaf boundaries.
    */
  private def getLeafBoundaryList(beamAl: AttributeList): Seq[Double] = {
    DicomUtil.findAllSingle(beamAl, TagByName.LeafPositionBoundaries).head.getDoubleValues
  }

  /**
    * Sort out the jaw an MLC values to simplify access.  If the
    * @param beamAl Attribute list of beam of interest.
    */
  private case class ControlPointSequence(beamAl: AttributeList, rtimage: AttributeList) {

    private case class PosSeq(posSeq: AttributeList) {
      private val devType: String = posSeq.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrNull.trim
      val posList: Seq[Double] = posSeq.get(TagByName.LeafJawPositions).getDoubleValues.toSeq // toSeq to make this immutable

      val isXMlc: Boolean = devType.equals("MLCX")
      val isXJaw: Boolean = devType.equals("ASYMX") || devType.equals("X")
      val isYJaw: Boolean = devType.equals("ASYMY") || devType.equals("Y")
    }

    private val rtimageGantryAngle = DicomUtil.findAllSingle(rtimage, TagByName.GantryAngle).head.getDoubleValues.head

    // use the ControlPointSequence that has a gantry angle closest to the gantry angle of the RTIMAGE
    private val ControlPointSequence = DicomUtil.seqToAttr(beamAl, TagByName.ControlPointSequence).minBy(c => (c.get(TagByName.GantryAngle).getDoubleValues.head - rtimageGantryAngle).abs)

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

    def mlcX1Pos(index: Int) = if ((index >= 0) && (index < mlcX1PosList.size)) Some(mlcX1PosList(index)) else None

    val mlcX2PosList: Seq[Double] = posSeq.find(_.isXMlc) match {
      case Some(ps) => ps.posList.takeRight(ps.posList.size / 2)
      case _        => Seq()
    }

    def mlcX2Pos(index: Int) = if ((index >= 0) && (index < mlcX2PosList.size)) Some(mlcX2PosList(index)) else None

  }

  /** Indicates that there is no leaf adjacent to a leaf side. */
  private val noLeaf: Double = -1000000

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
      scale: Double = 1,
      color: Color = new Color(255, 255, 0),
      font: Font = new Font(ImageText.DefaultFont, Font.PLAIN, ImageText.DefaultTextPointSize)
  ): BufferedImage = {

    val beamAl = getBeamOfRtimage(rtplan, rtimage)

    val cps = ControlPointSequence(beamAl, rtimage)

    val leafBoundaryList = getLeafBoundaryList(beamAl)
    Trace.trace("leafBoundaryList: " + leafBoundaryList.mkString("  "))

    val image = ImageUtil.magnify(initialImage, scale.round.toInt)
    // val image = ImageUtil.deepCopy(initialImage)

    val gc = ImageUtil.getGraphics(image)
    gc.setColor(color)
    gc.setFont(font)

    val trans = new IsoImagePlaneTranslator(rtimage)

    val scaleOffset = (scale - 1) / 2

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
    def drawLine(x1: Double, y1: Double, x2: Double, y2: Double): Unit = {
      val p1 = transPoint(new Point2d(x1, y1))
      val p2 = transPoint(new Point2d(x2, y2))
      gc.drawLine(p1.getX, p1.getY, p2.getX, p2.getY)
    }

    /**
      * Determine if leaf boundary (side) is within the bounds of the jaws and is therefore visible.
      * @param index Index of leaf boundary.
      * @return True if visible.
      */
    def leafBoundaryInsideJaws(index: Int): Boolean = {
      val leafBoundary = leafBoundaryList(index)
      val inBounds =
        ((cps.y1Jaw.isDefined && cps.y1Jaw.get < leafBoundary) || cps.y1Jaw.isEmpty) &&
          ((cps.y2Jaw.isDefined && cps.y2Jaw.get > leafBoundary) || cps.y2Jaw.isEmpty)
      inBounds
    }

    def leafPositionInsideJaws(leafPos: Double): Boolean = {
      val inBounds =
        ((cps.x1Jaw.isDefined && cps.x1Jaw.get < leafPos) || cps.x1Jaw.isEmpty) &&
          ((cps.x2Jaw.isDefined && cps.x2Jaw.get > leafPos) || cps.x2Jaw.isEmpty)
      inBounds
    }

    /**
      * Draw a single X1 MLC leaf and its end.
      *
      * @param index Index of leaf.
      */
    def drawX1Leaf(index: Int): Unit = {
      // draw the leaf boundary (side of leaf)
      if (leafBoundaryInsideJaws(index)) {
        val posBefore = cps.mlcX1Pos(index - 1)
        val posAfter = cps.mlcX1Pos(index)
        val oppPosBefore = cps.mlcX2Pos(index - 1)
        val oppPosAfter = cps.mlcX2Pos(index)
        val jaw = cps.x1Jaw
        val oppJaw = cps.x2Jaw

        val x1 = Seq(posBefore, jaw, posAfter)

        val largest = Seq(posBefore, posAfter, jaw).flatten.sorted.tail
        val y = leafBoundaryList(index)

        // X2 limit
        val maxX: Double = {
          val opposing = if (index < cps.mlcX2PosList.size) Some(cps.mlcX2PosList(index)) else None
          Seq(cps.x2Jaw, opposing, Some(-noLeaf)).flatten.min
          //Seq(cps.x2Jaw, Some(-noLeaf)).flatten.min
        }

        drawLine(Math.min(largest.head, maxX), y, Math.min(largest.last, maxX), y)
      }
      //drawLine(jaw, y, largest.last, y)

      // draw the end of the leaf
      if (
        (index < (leafBoundaryList.size - 1)) &&
        leafPositionInsideJaws(cps.mlcX1PosList(index)) &&
        (leafBoundaryInsideJaws(index) || leafBoundaryInsideJaws(index + 1))
      ) {
        val y1 = if (leafBoundaryInsideJaws(index)) leafBoundaryList(index) else cps.y1Jaw.get
        val y2 = if (leafBoundaryInsideJaws(index + 1)) leafBoundaryList(index + 1) else cps.y2Jaw.get

        drawLine(cps.mlcX1PosList(index), y1, cps.mlcX1PosList(index), y2)
      }

    }

    /**
      * Draw a single X1 MLC leaf and its end.
      *
      * @param index Index of leaf.
      */
    def drawX2Leaf(index: Int): Unit = {
      if (leafBoundaryInsideJaws(index)) {
        val before = if (index < 1) -noLeaf else cps.mlcX2PosList(index - 1)
        val after = if (index >= cps.mlcX2PosList.size) noLeaf else cps.mlcX2PosList(index)
        val jaw = if (cps.x2Jaw.isDefined) cps.x2Jaw.get else -noLeaf
        val smallest = Seq(before, after, jaw).sorted.reverse.tail
        val y = leafBoundaryList(index)

        // far jaw limit
        // val minX = if (cps.x1Jaw.isEmpty) smallest.max else cps.x1Jaw.get
        // X1 limit
        val minX: Double = {
          val opposing = if (index < cps.mlcX1PosList.size) Some(cps.mlcX1PosList(index)) else None
          Seq(cps.x1Jaw, opposing, Some(noLeaf)).flatten.max
          //Seq(cps.x2Jaw, Some(-noLeaf)).flatten.min
        }

        drawLine(Math.max(smallest.head, minX), y, Math.max(smallest.last, minX), y)
      }
      //drawLine(jaw, y, largest.last, y)

      // draw the end of the leaf
      if (
        (index < (leafBoundaryList.size - 1)) &&
        leafPositionInsideJaws(cps.mlcX2PosList(index)) &&
        (leafBoundaryInsideJaws(index) || leafBoundaryInsideJaws(index + 1))
      ) {
        val y1 = if (leafBoundaryInsideJaws(index)) leafBoundaryList(index) else cps.y1Jaw.get
        val y2 = if (leafBoundaryInsideJaws(index + 1)) leafBoundaryList(index + 1) else cps.y2Jaw.get

        drawLine(cps.mlcX2PosList(index), y1, cps.mlcX2PosList(index), y2)
      }

    }

    def drawMlc(): Unit = {
      leafBoundaryList.indices.foreach(drawX1Leaf)
      leafBoundaryList.indices.foreach(drawX2Leaf)
    }

    def drawXJaw(): Unit = {
      // TODO
    }

    def drawYJaw(): Unit = {
      // TODO
    }

    /**
      * Draw a simple rectangle outlining the jaws.
      */
    def drawBothJaws(): Unit = {
      drawLine(cps.x1Jaw.get, cps.y2Jaw.get, cps.x2Jaw.get, cps.y2Jaw.get) // top
      drawLine(cps.x1Jaw.get, cps.y1Jaw.get, cps.x2Jaw.get, cps.y1Jaw.get) // bottom
      drawLine(cps.x1Jaw.get, cps.y2Jaw.get, cps.x1Jaw.get, cps.y1Jaw.get) // left
      drawLine(cps.x2Jaw.get, cps.y1Jaw.get, cps.x2Jaw.get, cps.y2Jaw.get) // right
    }

    (cps.x1Jaw.isDefined, cps.y1Jaw.isDefined, cps.mlcX1PosList.nonEmpty) match {
      case (true, true, true) =>
        drawBothJaws()
        drawMlc()
      case (true, true, false) =>
        drawBothJaws()
      case (true, false, true) =>
        drawXJaw()
        drawMlc()
      case (true, false, false) =>
        drawXJaw()
      case (false, true, true) =>
        drawYJaw()
        drawMlc()
      case (false, true, false) =>
        drawYJaw()
      case (false, false, true) =>
        drawMlc()
      case (false, false, false) =>
      // draw nothing
    }

    image
  }
}

object DrawBEV extends Logging {
  def main(args: Array[String]): Unit = {

    def readDicom(dir: File, fileName: String): AttributeList = {
      val al = new AttributeList
      val file = new File(dir, fileName)
      al.read(file)
      al
    }

    println("Starting...")
    val start = System.currentTimeMillis()

    if (true) {
      Trace.trace()
      println(TagByName.BeamSequence)
      Trace.trace()
    }

    val dir = new File("""D:\tmp\aqa\tmp\017076541\looking""")
    val outDir = new File(dir, "test")
    val rtplan = readDicom(dir, "RTPLAN.dcm")
    val rtimage = readDicom(dir, "RTIMAGE.dcm")

    val di = new DicomImage(rtimage)
    val bufImg = di.toBufferedImage(Color.black)

    val drawBEV = DrawBEV(rtplan, rtimage)

    val image = drawBEV.draw(bufImg, scale = 2)

    val pngName = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss").format(new Date) + ".png"

    val outPng = new File(outDir, pngName)
    ImageUtil.writePngFile(image, outPng)
    println("Wrote file to : " + outPng.getAbsolutePath)

    val elapsed = System.currentTimeMillis() - start
    println(s"Done.  Elapsed ms: $elapsed")
    System.exit(0)
  }
}
