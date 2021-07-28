/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package learn

import edu.umro.ImageUtil.LocateEdge

import java.io.File
import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage

import java.awt.Rectangle
import com.pixelmed.dicom.TagFromName
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.scalactic.source.Position.apply
import org.scalatest.Finders
import edu.umro.util.UMROGUID
import edu.umro.ImageUtil.ImageUtil

import javax.vecmath.Point2d
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ImageUtil.LocateMax
import org.opensourcephysics.numerics.CubicSpline
import edu.umro.ImageUtil.IsoImagePlaneTranslator

object ConstructIdealDicom {

  val dir = new File("""src/test/resources/TestLocateEdge""")

  val outDir = new File("target/TestLocateEdge")
  outDir.mkdirs

  def fmt(d: Double) = d.formatted("%20.15f")

  def readDicomFile(file: File) = {
    val al = new AttributeList
    al.read(file)
    al
  }

  val MSK_WL_G090_C090file = new File(dir, "MSK_WL_G090_C090.dcm")
  val MSK_WL_G090_C270file = new File(dir, "MSK_WL_G090_C270.dcm")

  val MSK_TB444_C270_6Xfile = new File(dir, "MSK_TB444_C270_6X.dcm")
  val MSK_TB444_C090_6Xfile = new File(dir, "MSK_TB444_C090_6X.dcm")
  val MSK_TB445_C270_6Xfile = new File(dir, "MSK_TB445_C270_6X.dcm")
  val MSK_TB445_C090_6Xfile = new File(dir, "MSK_TB445_C090_6X.dcm")

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

  def leftEdgeBoundingRectangle(image: DicomImage) = {
    val s = 200
    val midW = image.width / 2
    val midH = image.height / 2
    val top = image.getSubimage(new Rectangle(midW - s, midH - border, s * 2, border * 2))
  }

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
    val RadiationMachineSAD = al.get(TagByName.RadiationMachineSAD)
    RadiationMachineSAD.removeValues
    RadiationMachineSAD.addValue(1000.0)
  }

  def sidTo1500(al: AttributeList) = {
    val RTImageSID = al.get(TagByName.RTImageSID)
    RTImageSID.removeValues
    RTImageSID.addValue(1500.0)
  }

  def idealize(al: AttributeList): Unit = {
    modSop(al)
    sadTo1000(al)
    sidTo1500(al)
  }

  def setCollAngle(al: AttributeList, collimatorAngle: Double) = {
    val list = DicomUtil.findAllSingle(al, TagByName.BeamLimitingDeviceAngle)

    list.map(at => {
      at.removeValues
      at.addValue(collimatorAngle)
    })
  }

  def setGantryAngle(al: AttributeList, gantryAngle: Double) = {
    val list = DicomUtil.findAllSingle(al, TagByName.GantryAngle)

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

  def centerOfMass(file: File) = {
    val al = readDicomFile(file)
    val di = new DicomImage(al)
    val trans = new IsoImagePlaneTranslator(al)

    val x = ImageUtil.centerOfMass(di.columnSums)
    val y = ImageUtil.centerOfMass(di.rowSums)

    val isoplane = trans.pix2Iso(x, y)

    def fmt(d: Double) = d.formatted("%20.15f")
    println(file.getName.formatted("%20s") + "  x,y " + fmt(x) + ", " + fmt(y) + "    image plane: " + fmt(isoplane.getX) + ", " + fmt(isoplane.getY))
  }

  def edgeBySlopes(al: AttributeList, rect: Rectangle): Unit = {
    val border = 40
    val di = new DicomImage(al)
    val leftRect = di.getSubimage(rect)
    val profile = {
      val raw = leftRect.columnSums
      val min = raw.min
      val range = raw.max - min
      val scale = raw.size / range

      raw.map(r => (r - min) * scale)
    }

    val spline = new CubicSpline(profile.indices.toArray.map(s => s.toDouble), profile.map(f => f.toDouble).toArray)

    def toSlope(i: Int, incr: Double) = {
      val x = i * incr
      val xLo = x - incr
      val xHi = x + incr

      val yLo = spline.evaluate(xLo)
      val yHi = spline.evaluate(xHi)

      val slope = (yHi - yLo) / (xHi - xLo)
      (x, slope)
    }

    val steepestZoom = 10000
    val steepestIncr = 1.0 / steepestZoom
    val steepestX = (0 until (profile.size * steepestZoom)).map(i => toSlope(i, steepestIncr)).maxBy(_._2.abs)._1

    val points = {
      val zoom = 100000
      val incr = 1.0 / zoom
      (1 until (profile.size * zoom)).map(i => toSlope(i, incr))
    }
    val best = points.sortBy(xy => (1 - xy._2.abs).abs).take(10).sortBy(_._1)

    if (false) {
      val zoom = 20
      val incr = 1.0 / zoom
      println("slope profile")
      (1 until (profile.size * zoom)).map(i => toSlope(i, incr)).map(is => println(fmt(is._2)))
    }

    if (false) {
      val zoom = 20
      val incr = 1.0 / zoom
      println("gradient profile")
      (1 until (profile.size * zoom)).map(i => spline.evaluate((i * incr) + incr) - spline.evaluate(i * incr)).map(g => println(fmt(g)))
    }

    val bestText = best.map(xy => fmt(xy._1) + ",  " + fmt(xy._2))
    val lo = best.filter(_._1 < steepestX).head._1
    val hi = best.filter(_._1 > steepestX).head._1
    //println(profile.map(y => fmt(y)).mkString("\n"))
    val avg = ((lo + hi) / 2)

    val mid = {
      val loY = spline.evaluate(lo)
      val hiY = spline.evaluate(hi)
      val midY = (loY + hiY) / 2
      points.minBy(m => (spline.evaluate(m._1) - midY).abs)._1
    }

    println("\nsteepestX: " + fmt(steepestX) +
      "    lo: " + fmt(lo) + "  hi: " + fmt(hi) +
      "    avg: " + fmt(avg) + " ==> " + fmt(avg + rect.getX) +
      "    mid: " + fmt(mid) + " ==> " + fmt(mid + rect.getX))
  }

  def main(args: Array[String]): Unit = {

    val start = System.currentTimeMillis
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

    if (false) {
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

    if (false) {
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

    if (false) {
      Seq(
        MSK_TB444_C270_6Xfile,
        MSK_TB444_C090_6Xfile,
        MSK_TB445_C270_6Xfile,
        MSK_TB445_C090_6Xfile).sortBy(_.getName).map(f => centerOfMass(f))
    }

    if (false) {

      val border = 40
      val width = 1190
      val height = 1190

      val top = 374
      val bottom = height - top
      val left = 374
      val right = width - left

      val leftRect = new Rectangle(left - border, top + border, border * 2, (bottom - top) - (border * 2))
      val rightRect = new Rectangle(right - border, top + border, border * 2, (bottom - top) - (border * 2))

      edgeBySlopes(readDicomFile(MSK_TB444_C090_6Xfile), leftRect)
      edgeBySlopes(readDicomFile(MSK_TB444_C090_6Xfile), rightRect)

      println

      edgeBySlopes(readDicomFile(MSK_TB444_C270_6Xfile), leftRect)
      edgeBySlopes(readDicomFile(MSK_TB444_C270_6Xfile), rightRect)

    }

    if (true) {
      val trans = new IsoImagePlaneTranslator(readDicomFile(MSK_TB444_C090_6Xfile))
      println("isoplane X center avg: " + fmt(trans.pix2Iso(595.0307675, 1190 / 2.0).getX))

      println("isoplane X center mid: " + fmt(trans.pix2Iso(594.46335, 1190 / 2.0).getX))

      println("isoplane eyeballed: " + trans.pix2Iso(594.5, 592.0))
      println("isoplane eyeballed: " + trans.pix2Iso(594.75, 592.25))
    }

    val elapsed = System.currentTimeMillis - start
    println("done. elapsed ms: " + elapsed)
  }

}
