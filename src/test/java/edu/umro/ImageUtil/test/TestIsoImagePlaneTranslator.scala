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

package edu.umro.ImageUtil.test;

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.IsoImagePlaneTranslator

import java.io.File

object TestIsoImagePlaneTranslator {

  val inDir = new File("""src\test\resources\TestIsoImagePlaneTranslator""")

  def check(name: String, cur: Double, old: Double) {
    val diff = cur - old
    def fmt(d: Double) = d.formatted("%14.8f")
    println(name.format("%-16s") + " cur: " + fmt(cur) + "     old: " + fmt(old) + "    diff: " + fmt(diff))
    if (diff.abs > 0.00001) {
      Thread.sleep(100)
      throw new RuntimeException("diff is too large")
    }
  }

  def testFile(file: File) = {
    val al = new AttributeList
    al.read(file)
    println("\nTesting file " + file.getAbsolutePath)

    val ipt = new IsoImagePlaneTranslator(al)
    val iptOld = new IsoImagePlaneTranslatorOld(al)

    check("iso2PixCoordX", ipt.iso2PixCoordX(0), iptOld.iso2PixCoordX(0))

    for (v <- (-10000 to 10000)) {
      val d = v / 10.0
      check("iso2PixDistX", ipt.iso2PixDistX(d), iptOld.iso2PixDistX(d))
      check("iso2PixCoordX", ipt.iso2PixCoordX(d), iptOld.iso2PixCoordX(d))
      check("iso2PixDistY", ipt.iso2PixDistY(d), iptOld.iso2PixDistY(d))
      check("iso2PixCoordY", ipt.iso2PixCoordY(d), iptOld.iso2PixCoordY(d))

      check("pix2IsoDistX", ipt.pix2IsoDistX(d), iptOld.pix2IsoDistX(d))
      check("pix2IsoCoordX", ipt.pix2IsoCoordX(d), iptOld.pix2IsoCoordX(d))
      check("pix2IsoDistY", ipt.pix2IsoDistY(d), iptOld.pix2IsoDistY(d))
      check("pix2IsoCoordY", ipt.pix2IsoCoordY(d), iptOld.pix2IsoCoordY(d))
    }
  }

  def main(args: Array[String]): Unit = {
    inDir.listFiles.toSeq.map(f => testFile(f))
    println("all tests passed")
  }

}