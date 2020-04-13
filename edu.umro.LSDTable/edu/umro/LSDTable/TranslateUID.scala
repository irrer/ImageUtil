package edu.umro.LSDTable

import edu.umro.util.UMROGUID
import com.pixelmed.dicom.Attribute

/**
 * Translate UIDs from original to new.  Used to make new versions of files.
 */
object TranslateUID {
  private val uidMap = scala.collection.mutable.HashMap[String, String]()

  private def getNewUid(oldUid: String): String = {
    if (uidMap.contains(oldUid)) uidMap(oldUid)
    else {
      val newUid = UMROGUID.getUID
      uidMap.put(oldUid, newUid)
      newUid
    }
  }

  def updateAttrUid(attr: Attribute) = {
    val oldUid = attr.getSingleStringValueOrEmptyString
    val newUid = TranslateUID.getNewUid(oldUid)
    attr.removeValues
    attr.addValue(newUid)
  }

}