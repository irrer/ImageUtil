package learn

import edu.umro.ImageUtil.LocateEdge
import java.io.File
import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import java.awt.Rectangle
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.DicomUtil
import org.scalactic.source.Position.apply
import org.scalatest.Finders
import edu.umro.util.UMROGUID
import edu.umro.ImageUtil.ImageUtil
import javax.vecmath.Point2d

object ConstructIdealDicom {

  val dir = new File("""src/test/resources/TestLocateEdge""")

  val outDir = new File("target/TestLocateEdge")
  outDir.mkdirs

  def readDicomFile(file: File) = {
    val al = new AttributeList
    al.read(file)
    al
  }

  val MSK_WL_G090_C090file = new File(dir, "MSK_WL_G090_C090.dcm")
  val MSK_WL_G090_C270file = new File(dir, "MSK_WL_G090_C270.dcm")

  def writeDicomFile(al: AttributeList, name: String): Unit = {

    val fileName = if (name.toLowerCase.endsWith(".dcm")) name else name + ".dcm"

    val outFile = new File(outDir, fileName)
    outFile.delete
    DicomUtil.writeAttributeListToFile(al, outFile, "AQA TestLocateEdge")
    println("wrote file to " + outFile.getAbsolutePath)
  }

  // MSK Winston Lutz image characteristics in pixels and CU
  val topEdge = 600
  val bottomEdge = 680
  val leftEdge = 600
  val rightEdge = 680
  val border = 20
  val ballDiam = 35.0
  val ballRadius = ballDiam / 2

  val ballBackgroundCU = 3380.toShort
  val ballCenterCU = 2870.toShort
  val backgroundCU = 2.toShort

  def measureLeftEdge(image: DicomImage) = {
    println("----------------------------------------------")

    val s = 200
    val midW = image.width / 2
    val midH = image.height / 2
    val top = image.getSubimage(new Rectangle(midW - s, midH - border, s * 2, border * 2))
    //println(top.columnSums.mkString("\n    ", "\n    ", ""))

    val ht = (bottomEdge - border) - (topEdge + border)
    val x = leftEdge - border
    val sub = image.getSubimage(new Rectangle(x, topEdge + border, border * 2, ht))
    val colSums = sub.columnSums.map(s => s / ht)
    println(colSums.mkString("\n    ", "\n    ", ""))
    val threshold = (colSums.max + colSums.min) / 2.0
    println("Left edge at: " + (x + LocateEdge.locateEdge(colSums, threshold)))
  }

  def horizontalPixelsAcrossCenter(al: AttributeList) = {
    val di = new DicomImage(al)
    val mid = di.Rows / 2
    val text = (0 until di.Columns).map(c => di.get(c, mid).formatted("%6.0f")).mkString("\n    ")
    println(text)
  }

  def showPixelGrid(al: AttributeList, top: Int, bottom: Int, left: Int, right: Int) = {
    val di = new DicomImage(al)
    for (y <- top until bottom) {
      for (x <- left until right) {
        print(di.get(x, y).round.toInt.formatted("%6d"))
      }
      println
    }
  }

  // -------------------------------------------------------

  def modSop(al: AttributeList) = {
    val at = al.get(TagFromName.SOPInstanceUID)
    val sop = at.getSingleStringValueOrEmptyString
    at.removeValues
    at.addValue(UMROGUID.getUID)
  }

  def sadTo1000(al: AttributeList) = {
    val RadiationMachineSAD = al.get(TagFromName.RadiationMachineSAD)
    RadiationMachineSAD.removeValues
    RadiationMachineSAD.addValue(1000.0)
  }

  def sidTo1500(al: AttributeList) = {
    val RTImageSID = al.get(TagFromName.RTImageSID)
    RTImageSID.removeValues
    RTImageSID.addValue(1500.0)
  }

  def idealize(al: AttributeList): Unit = {
    modSop(al)
    sadTo1000(al)
    sidTo1500(al)
  }

  def setCollAngle(al: AttributeList, collimatorAngle: Double) = {
    val list = DicomUtil.findAllSingle(al, TagFromName.BeamLimitingDeviceAngle)

    list.map(at => {
      at.removeValues
      at.addValue(collimatorAngle)
    })
  }

  def setGantryAngle(al: AttributeList, gantryAngle: Double) = {
    val list = DicomUtil.findAllSingle(al, TagFromName.GantryAngle)

    list.map(at => {
      at.removeValues
      at.addValue(gantryAngle)
    })
  }

  // make AttributeList without ball
  def makeIdealBackground: AttributeList = {
    val al = readDicomFile(MSK_WL_G090_C090file)
    idealize(al)

    val width = al.get(TagFromName.Columns).getIntegerValues.head
    val height = al.get(TagFromName.Rows).getIntegerValues.head

    val pixAttr = al.get(TagFromName.PixelData)
    val pixels = pixAttr.getShortValues

    pixels.indices.map(i => pixels(i) = backgroundCU)

    al
  }

  // make AttributeList without ball
  def makeIdealBackgroundEdged: AttributeList = {
    val al = readDicomFile(MSK_WL_G090_C090file)
    idealize(al)

    val width = al.get(TagFromName.Columns).getIntegerValues.head
    val height = al.get(TagFromName.Rows).getIntegerValues.head

    val pixAttr = al.get(TagFromName.PixelData)
    val pixels = pixAttr.getShortValues

    for (y <- 0 until height; x <- 0 until width) {
      val i = (y * width) + x
      val e = (y == 0) || (x == 0) || (y == (height - 1)) || (x == (width - 1))
      pixels(i) = if (e) ballBackgroundCU else backgroundCU
    }
    al
  }

  // make AttributeList without ball
  def makeIdealAlWithoutBall: AttributeList = {
    val al = readDicomFile(MSK_WL_G090_C090file)
    idealize(al)

    val width = al.get(TagFromName.Columns).getIntegerValues.head
    val height = al.get(TagFromName.Rows).getIntegerValues.head

    val pixAttr = al.get(TagFromName.PixelData)
    val pixels = pixAttr.getShortValues

    for (y <- 0 until height; x <- 0 until width) {
      val p = if ((y > topEdge) && (y < (height - (topEdge + 1))) && (x > leftEdge) && (x < (width - (leftEdge + 1)))) ballBackgroundCU else backgroundCU
      val i = (y * width) + x
      pixels(i) = p
    }
    al
  }

  def centerOfMass(al: AttributeList) = {
    val di = new DicomImage(al)
    val x = ImageUtil.centerOfMass(di.columnSums)
    val y = ImageUtil.centerOfMass(di.rowSums)
    x.formatted("%20.15f") + ", " + y.formatted("%20.15f")
  }

  // get the CU reading a the given radius of the ball
  def ballThicknessCU(radius: Double): Short = {
    if (radius > ballRadius)
      ballBackgroundCU
    else {
      try {
        val max = ballBackgroundCU - ballCenterCU
        val vert = Math.sqrt((ballRadius * ballRadius) - (radius * radius)) * 2 // distance from top to bottom of ball

        val thickness = ballBackgroundCU - ((vert / ballDiam) * max)
        thickness.round.toShort
      } catch {
        case t: Throwable =>
          {
            println("got a ballThicknessCU error with radius " + radius + " : " + t)
            ballBackgroundCU
          }
      }
    }
  }

  // make AttributeList with ball
  def makeIdealAlWithBall: AttributeList = {
    val al = makeIdealAlWithoutBall

    idealize(al)

    val width = al.get(TagFromName.Columns).getIntegerValues.head
    val height = al.get(TagFromName.Rows).getIntegerValues.head
    val pixAttr = al.get(TagFromName.PixelData)
    val pixels = pixAttr.getShortValues

    val xCntr = (width / 2) - 0.5
    val yCntr = (height / 2) - 0.5

    for (y <- 0 until height; x <- 0 until width) {
      val xd = x - xCntr
      val yd = y - yCntr

      val dist = Math.sqrt((xd * xd) + (yd * yd))
      if (dist < ballRadius) {
        val i = (y * width) + x
        pixels(i) = ballThicknessCU(dist)
      }
    }
    al
  }

  def testBallThicknessCU = {
    for (t <- ((-3) to ballRadius.toInt + 5)) {
      println(t.formatted("%4d") + " : " + ballThicknessCU(t).formatted("%5d"))
    }
  }

  def main(args: Array[String]): Unit = {
    if (false) {
      val fileList = Seq(MSK_WL_G090_C090file, MSK_WL_G090_C270file)
      val alList = fileList.map(file => readDicomFile(file))
      val imageList = alList.map(al => new DicomImage(al))
      imageList.map(image => measureLeftEdge(image))
    }

    if (false) testBallThicknessCU

    if (false) {
      val al = makeIdealAlWithBall
      writeDicomFile(al, "MSK_WL_G090_C090withBall")
    }

    if (false) {
      for (ga <- Seq(0, 90, 180, 270); co <- Seq(0, 90, 180, 270)) {
        val al = makeIdealAlWithBall
        setGantryAngle(al, ga)
        setCollAngle(al, co)
        writeDicomFile(al, "MSK-WL-IdealG" + ga.formatted("%03d") + "-C" + co.formatted("%03d"))
      }
    }

    if (false) {
      val al = readDicomFile(MSK_WL_G090_C090file)
      val b = 10
      showPixelGrid(al, topEdge - b, bottomEdge + b, leftEdge - b, rightEdge + b)
      println("=============")
    }

    if (false) {
      val al = makeIdealAlWithBall
      val b = 10
      showPixelGrid(al, topEdge - b, bottomEdge + b, leftEdge - b, rightEdge + b)
    }

    if (true) {
      val constA = Seq(5, 5, 5, 5, 5, 5, 5, 5, 5, 5).map(i => i.toFloat).toIndexedSeq
      println("Center of mass of constA size " + constA.size + " : " + ImageUtil.centerOfMass(constA))

      val constB = constA :+ constA.head
      println("Center of mass of constB size " + constB.size + " : " + ImageUtil.centerOfMass(constB))

      println("Center of mass of ideal background: " + centerOfMass(makeIdealBackground))

      println("Center of mass of ideal without ball: " + centerOfMass(makeIdealAlWithoutBall))

      println("Center of mass of ideal with ball: " + centerOfMass(makeIdealAlWithBall))

      println("Center of mass of MSK_WL_G090_C090: " + centerOfMass(readDicomFile(MSK_WL_G090_C090file)))

      println("Center of mass of MSK_WL_G090_C270: " + centerOfMass(readDicomFile(MSK_WL_G090_C270file)))
    }

    if (false) {
      val edged = makeIdealBackgroundEdged

      println("top left")
      showPixelGrid(edged, 0, 10, 0, 10)
      println
      println("top right")
      showPixelGrid(edged, 0, 10, 1270, 1280)
      println

      println("Center of mass of ideal edged: " + centerOfMass(edged))
      writeDicomFile(edged, "edged")
    }

    if (true) {
      val edged = new DicomImage(makeIdealBackgroundEdged)
      println("row sums    : " + edged.rowSums)
      println("column sums : " + edged.columnSums)
      println("row com     : " + ImageUtil.centerOfMass(edged.rowSums).formatted("%20.15f"))
      println("column com  : " + ImageUtil.centerOfMass(edged.columnSums).formatted("%20.15f"))
    }

    if (false) {
      val constA = Seq(4326400.0, 9316.0, 9316.0, 9316.0, 9316.0, 9316.0, 9316.0, 4326400.0).map(i => i.toFloat).toIndexedSeq
      println("Center of mass of constA size " + constA.size + " : " + ImageUtil.centerOfMass(constA))
    }

  }

}
