package edu.umro.ImageUtil

import java.io.File
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.SOPClass

object DicomToDeepColorPng {

  /**
   * Safely get a list of files in a directory.  On failure, return an empty list.
   */
  private def listFiles(dir: File): List[File] = {
    try {
      dir.listFiles.toList
    } catch {
      case t: Throwable => List[File]()
    }
  }

  private def listRegularFiles(file: File): Seq[File] = {
    if (file.isFile) Seq(file)
    else {
      val fList = listFiles(file)
      fList.map(f => listRegularFiles(f)).flatten
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
      case t: Throwable => None
    }
  }

  private def toPng(file: File, al: AttributeList) = {
    val di = new DicomImage(al)
    val bufImage = di.toDeepColorBufferedImage(0.01)
    val imageFile = {
      val fileName = file.getName
      val imageFileName = if (fileName.toLowerCase.endsWith(".dcm")) {
        fileName.dropRight(4) + ".png"
      } else
        fileName + ".dcm"
      new File(file.getParentFile, imageFileName)
    }
    imageFile.delete
    println("Creating " + imageFile.getAbsolutePath)
    ImageUtil.writePngFile(bufImage, imageFile)
  }

  private def isImageStg(al: AttributeList) = {
    val c = al.get(TagFromName.SOPClassUID).getSingleStringValueOrEmptyString
    SOPClass.isImageStorage(c)
  }

  def main(args: Array[String]): Unit = {
    val list = args.map(fileName => listRegularFiles(new File(fileName))).flatten.map(file => getImageAl(file)).flatten.filter(fa => isImageStg(fa._2))
    list.map(fd => toPng(fd._1, fd._2))
  }
}