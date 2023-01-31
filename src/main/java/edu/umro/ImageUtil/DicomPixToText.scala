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

package edu.umro.ImageUtil

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.SOPClass
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.FileUtil

import java.io.File

/**
  * End user utility to dump the pixels of DICOM image files as a text file formatted as a 2 dimensional array of numbers.
  *
  * Specify directories and DICOM files as command lines.  It will recursively descend directories.
  */
object DicomPixToText {

  /**
    * Safely get a list of files in a directory.  On failure, return an empty list.
    */
  private def listFiles(dir: File): List[File] = {
    try {
      dir.listFiles.toList
    } catch {
      case _: Throwable => List[File]()
    }
  }

  private def listRegularFiles(file: File): Seq[File] = {
    if (file.isFile) Seq(file)
    else {
      val fList = listFiles(file)
      fList.flatMap(f => listRegularFiles(f))
    }
  }

  /**
    * Get the attribute list if this is a DICOM image file, otherwise return None.
    */
  private def getImageAl(file: File): Option[(File, AttributeList)] = {
    try {
      val al = new AttributeList
      al.read(file)
      if (al.get(TagFromName.PixelData) == null)
        None
      else
        Some(file, al)
    } catch {
      case _: Throwable => None
    }
  }

  private def convertToText(file: File, al: AttributeList): Unit = {
    val di = new DicomImage(al)
    val text = di.pixelsToText

    val textFile = {
      val fileName = file.getName
      val imageFileName = if (fileName.toLowerCase.endsWith(".dcm")) {
        fileName.dropRight(4) + "_.txt"
      } else
        fileName + ".dcm"
      new File(file.getParentFile, imageFileName)
    }
    textFile.delete
    FileUtil.writeFile(textFile, text)
    println("Wrote file " + textFile.getAbsolutePath)
  }

  private def isImageStg(al: AttributeList) = {
    val c = al.get(TagFromName.SOPClassUID).getSingleStringValueOrEmptyString
    SOPClass.isImageStorage(c)
  }

  def main(args: Array[String]): Unit = {
    val list = args.flatMap(fileName => listRegularFiles(new File(fileName))).flatMap(file => getImageAl(file)).filter(fa => isImageStg(fa._2))
    list.foreach(fd => convertToText(fd._1, fd._2))
  }
}
