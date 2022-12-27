/*
 * Copyright 2022 Regents of the University of Michigan
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

package edu.umro.ImageUtil.test

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.DrawBEV
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ScalaUtil.FileUtil

import java.awt.Color
import java.io.File

/**
  * Test the DrawBEV class.  Also provides an example to follow.
  *
  * This is not an automatic test.  It requires a human to compare the results to a Varian image.
  */
object TestDrawBEV {

  val inDir = new File("src\\test\\resources\\TestDrawBEV")
  val outDir = new File("target\\TestDrawBEV")

  val rtimageFile = new File(inDir, "$TestDrawBEV_RTIMAGE.DCM")
  val rtplanFile = new File(inDir, "$TestDrawBEV_RTPLAN.DCM")

  private def readToAl(file: File): AttributeList = {
    val al = new AttributeList
    println("Reading DICOM file " + file.getAbsolutePath)
    al.read(file)
    al
  }

  def main(args: Array[String]): Unit = {

    println("Starting...")

    val rtimage = readToAl(rtimageFile)
    val rtplan = readToAl(rtplanFile)
    val varianPngName = "VarianScreenShot.png"
    val drawBev = DrawBEV(rtplan, rtimage)

    val di = new DicomImage(rtimage)
    val maxPix = (di.minPixelValue + di.maxPixelValue) * 1
    val bufImg = di.toBufferedImage(ImageUtil.rgbColorMap(Color.white), di.minPixelValue, maxPix)

    val image = drawBev.draw(bufImg, scale = 2)

    FileUtil.deleteFileTree(outDir)

    outDir.mkdirs()
    val outPngFile = new File(outDir, "DrawBEV.png")
    println("Writing file " + outPngFile.getAbsolutePath)
    ImageUtil.writePngFile(image, outPngFile)

    val varianPngIn = new File(inDir, varianPngName)
    val varianPngOut = new File(outDir, varianPngName)
    println("Copying Varian screen shot file output dir " + varianPngIn.getAbsolutePath + " --> " + varianPngOut.getAbsolutePath)
    val varianBinary = FileUtil.readBinaryFile(varianPngIn).right.get
    FileUtil.writeBinaryFile(varianPngOut, varianBinary)

    println("Compare the output PNG to the VarianScreenShot.png it is imitating.")
    println("Done.")
  }

}
