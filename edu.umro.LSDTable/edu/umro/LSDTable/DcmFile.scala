package edu.umro.LSDTable

import java.io.File
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.FileUtil
import java.util.Date
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.FloatSingleAttribute
import com.pixelmed.dicom.FloatDoubleAttribute
import com.pixelmed.dicom.Attribute

case class DcmFile(file: File) {
  LSDUtil.prnt("Reading " + file.getAbsolutePath)

  val al = new AttributeList
  al.read(file)

  def get(tag: AttributeTag): String = {
    try {
      new String(al.get(tag).getSingleStringValueOrEmptyString)
    } catch {
      case t: Throwable => ""
    }
  }

  private def getAllTableAngles(al: AttributeList): Seq[Attribute] = {
    val tableAngleTagSet = Set(
      TagFromName.TableTopEccentricAngle,
      TagFromName.TableTopPitchAngle,
      TagFromName.TableTopRollAngle)
    DicomUtil.findAll(al, tableAngleTagSet)
  }

  /**
   * Set all table angles in the given attribute list to zero.
   */
  def zeroTableAngles: Unit = {

    def zero(attr: Attribute) = {
      attr match {
        case flt: FloatSingleAttribute => {
          val size = flt.getFloatValues.size
          flt.removeValues
          (0 until size).map(i => flt.addValue(0.toFloat))
        }
        case dbl: FloatDoubleAttribute => {
          val size = dbl.getFloatValues.size
          dbl.removeValues
          (0 until size).map(i => dbl.addValue(0.toDouble))
        }
        case _ => {
          throw new Exception("Unknown table angle attribute type " + attr.getClass.getName + "  value: " + attr.toString.replace('\0', ' '))
        }
      }
    }

    getAllTableAngles(al).map(attr => zero(attr))
  }

  private def positionOf(al: AttributeList) = {
    val ipp = al.get(TagFromName.SliceLocation)
    if (ipp == null) 0 else ipp.getDoubleValues.head
  }

  /**
   * Absolute value of largest table angle.
   */
  val maxTableAngle: Double = {
    def sToDbl(s: String): Double = {
      try {
        s.toDouble
      } catch {
        case t: Throwable => 0.0
      }
    }

    val angleList =
      getAllTableAngles(al).
        map(attr => attr.getStringValues).
        flatten.
        map(s => sToDbl(s)).
        map(angle => angle.abs)
    if (angleList.isEmpty) 0 else angleList.max
  }

  val position = positionOf(al)

  def dateOfAl(al: AttributeList): Date = {
    val dateTimeTagPairList = List(
      (TagFromName.ContentDate, TagFromName.ContentTime),
      (TagFromName.SeriesDate, TagFromName.SeriesTime),
      (TagFromName.AcquisitionDate, TagFromName.AcquisitionTime),
      (TagFromName.CreationDate, TagFromName.CreationTime),
      (TagFromName.StudyDate, TagFromName.StudyTime),
      (TagFromName.InstanceCreationDate, TagFromName.InstanceCreationTime))

    val list = dateTimeTagPairList.map(dt => DicomUtil.getTimeAndDate(al, dt._1, dt._2))
    list.flatten.head
  }

  val date = {
    try {
      dateOfAl(al)
    } catch {
      case t: Throwable => {
        LSDUtil.future
      }
    }
  }

  val dateText = LSDUtil.dateFormat.format(new Date(date.getTime))

  override def toString = {
    get(TagFromName.PatientName) + " : " + get(TagFromName.PatientID) +
      get(TagFromName.Modality) +
      dateText + "  " +
      file.getAbsolutePath
  }
}
