package edu.umro.LSDTable

import java.io.File
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.Trace
import edu.umro.ScalaUtil.FileUtil
import edu.umro.LSDTable.LSDUtil._
import org.apache.logging.log4j.core.util.FileUtils
import edu.umro.util.Utility
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.AttributeFactory

object LSDTable {

  private val programName = "LSDTable"
  private val seriesDescription = "zero"

  private def usage(msg: String) = {
    prnt(msg)
    prnt("\nUsage: Drag and drop a folder containing DICOM files onto the " + programName + " icon.\n")
    System.exit(1)
    ??? // Tells the Scala compiler not to complain.  This is never executed because of the exit statement.
  }

  private def makeDcmFile(file: File): Option[DcmFile] = {
    try {
      Some(new DcmFile(file))
    } catch {
      case t: Throwable => {
        prnt("Ignoring as a non-DICOM file: " + file.getAbsolutePath)
        None
      }
    }
  }

  /**
   * Recursively descend a directory tree, acquiring a list of all non-directory files, DICOM and otherwise.
   */
  private def listRegularFiles(file: File): Seq[File] = {
    if (file.isFile) Seq(file)
    else {
      val fList = FileUtil.listFiles(file)
      fList.map(f => listRegularFiles(f)).flatten
    }
  }

  /**
   * Require all DICOM files to have the same patient.  If they don't, then print a message and return false.
   */
  private def allSameSamePatient(allDcmFile: Seq[DcmFile]) = {
    val patList = allDcmFile.groupBy(df => df.get(TagFromName.PatientID))
    if (patList.size > 1) {
      val patDesc = patList.map(fr => fr._2.head.toString).mkString("\n    ")
      usage("There are " + patList.size + " patients referenced, but there should be only one.  The differing files are:\n    " + patDesc)
    }
  }

  /**
   * Require all DICOM files to have the same frame of reference.  If they don't, then print a message and return false.
   */
  private def allSameFrameOfReference(allDcmFile: Seq[DcmFile]) = {
    def sameFOR(group: Seq[DcmFile]) = {
      val forList = group.groupBy(df => df.get(TagFromName.FrameOfReferenceUID))
      if (forList.size > 1) {
        val forDesc = forList.map(fr => fr._2.head.toString + " --> " + fr._2.head.get(TagFromName.FrameOfReferenceUID)).mkString("\n    ")
        usage("There are " + forList.size + " frames of reference, but there should be only one.  Differing files are:\n    " + forDesc)
      }
    }

    val modalityGroups = allDcmFile.groupBy(df => df.get(TagFromName.Modality))
    modalityGroups.map(mg => sameFOR(mg._2))
  }

  private def cullDuplicates(allDcmFile: Seq[DcmFile]): Seq[DcmFile] = {
    val groups = allDcmFile.groupBy(df => df.al.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString)
    val dupCount = allDcmFile.size - groups.size
    if (dupCount != 0) {
      val dupList = groups.map(g => g._2.tail).flatten
      val dupText = dupList.map(df => df.file.getAbsolutePath).mkString("\n    ")
      prnt("" + dupCount + " files were found with duplicate UIDs.  The following files are being ignored:\n    " + dupText)
      val uniqueList = groups.map(g => g._2.head)
      uniqueList.toSeq
    } else allDcmFile
  }

  /**
   * Establish the maximum table angle that will be allowed.  If a table angle is found whose absolute value is greater than this, then
   */
  private val MaxTableAngle: Double = {
    val envName = "MaxTableAngle"
    val defaultTableAngle = 1.0
    val text = System.getenv(envName)

    val angle: Double = if (text == null) {
      prnt("no " + envName + " environment variable specified.  Using default of " + defaultTableAngle)
      defaultTableAngle
    } else {
      try {
        val a = text.toDouble
        if (a < 0) {
          prnt("table angle specified by " + envName + " is " + text + " which is a negative floating point value.  Using default of " + defaultTableAngle)
          defaultTableAngle
        } else a
      } catch {
        case t: Throwable =>
          {
            prnt("table angle specified by " + envName + " is " + text + " which is not a valid floating point value.  Using default of " + defaultTableAngle)
            defaultTableAngle
          }
      }
    }

    prnt("Using " + envName + " of " + angle)
    angle
  }

  /**
   * Make sure all table angles are acceptably close to zero.
   */
  private def allTableAnglesNearZero(allDcmFile: Seq[DcmFile]) = {

    val tooBig = allDcmFile.filter(df => df.maxTableAngle > MaxTableAngle)
    if (tooBig.nonEmpty) {
      usage("The following file(s) have table angles whose absolute values are larger than " + MaxTableAngle + ":\n    " + tooBig.mkString("\n    "))
    }
  }

  /**
   * Use the input directory to establish the output directory.  Make the output a sibling of the input.
   */
  private def establishOutDir(inputDir: File): File = {
    val name = inputDir.getName + "-" + programName + "-output"
    val dir = new File(inputDir.getParentFile, name)

    try {
      Utility.deleteFileTree(dir)
      dir.mkdirs
    } catch {
      case t: Throwable => usage("Unable (not permitted?) to delete old output folder " + dir.getAbsolutePath + " : " + t)
    }

    if (!dir.isDirectory) usage("Unable (not permitted?) to create new output folder " + dir.getAbsolutePath)

    dir
  }

  private def establishInputDirectory(args: Array[String]): File = {
    0 match {
      case _ if args.isEmpty => usage("No input folder given.")
      case _ if args.size > 1 => usage("More than one folder given.")
      case _ if !((new File(args.head)).isDirectory) => usage(args.head + " is not a folder.")
      case _ => new File(args.head)
    }
  }

  private def updateDicom(dcmFile: DcmFile) = {
    def updateUIDs = {
      val uidTagSet = Set(
        TagFromName.SOPInstanceUID,
        TagFromName.SeriesInstanceUID,
        TagFromName.MediaStorageSOPInstanceUID,
        TagFromName.ReferencedSOPInstanceUID)

      val attrList = DicomUtil.findAll(dcmFile.al, uidTagSet)
      attrList.map(attr => TranslateUID.updateAttrUid(attr))
    }

    /**
     * Add an indication to the series description that this DICOM file was modified by this program.
     */
    def updateSeriesDescription = {
      val suffix = programName + " " + seriesDescription
      val attr = dcmFile.al.get(TagFromName.SeriesDescription)
      if (attr == null) {
        val sd = AttributeFactory.newAttribute(TagFromName.SeriesDescription)
        sd.addValue(suffix)
        dcmFile.al.put(sd)
      } else {
        val text = attr.getSingleStringValueOrEmptyString + " " + suffix
        attr.removeValues
        attr.addValue(text)
      }
    }

    updateUIDs
    updateSeriesDescription
    dcmFile.zeroTableAngles
  }

  private def write(outputDir: File, dcmFile: DcmFile): Unit = {
    val name = dcmFile.get(TagFromName.Modality) + dcmFile.get(TagFromName.MediaStorageSOPInstanceUID) + ".dcm"
    val outFile = new File(outputDir, name)
    DicomUtil.writeAttributeListToFile(dcmFile.al, outFile, programName)
  }

  def main(args: Array[String]): Unit = {

    val start = System.currentTimeMillis
    try {
      prnt("Linac Simulation Dicom editor for table pitch/roll/rotation correction (LSDtab)")

      val inputDir = establishInputDirectory(args)

      val allFiles = args.map(a => listRegularFiles(inputDir)).flatten
      prnt("number of files found: " + allFiles.size)

      val uniqueDcmFile = {
        val allDcmFile = allFiles.map(f => makeDcmFile(f)).flatten
        prnt("Number of DICOM files found: " + allDcmFile.size)
        cullDuplicates(allDcmFile)
      }
      prnt("Number of unique DICOM files found: " + uniqueDcmFile.size)

      allSameSamePatient(uniqueDcmFile)
      allSameFrameOfReference(uniqueDcmFile)
      allTableAnglesNearZero(uniqueDcmFile)

      val outputDir = establishOutDir(inputDir)
      prnt("Output folder: " + outputDir.getAbsolutePath)

      uniqueDcmFile.map(dcmFile => updateDicom(dcmFile))
      uniqueDcmFile.map(dcmFile => write(outputDir, dcmFile))
      prnt("Done.  Number of DICOM files written to " + outputDir.getAbsolutePath + " : " + uniqueDcmFile.size)
    } catch {
      case t: Throwable => {
        prnt("Elapsed ms: " + (System.currentTimeMillis - start) + "    Unexpected exception: " + t)
        t.printStackTrace
      }
    }
  }

}

