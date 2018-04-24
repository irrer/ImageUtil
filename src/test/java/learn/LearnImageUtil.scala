package edu.umro.ImageUtil

import com.pixelmed.dicom.AttributeList
import io.scif.img.SCIFIOImgPlus
import ij.gui.NewImage
import ij.process.ShortProcessor
import ij.ImageStack
import com.pixelmed.dicom.TagFromName
import io.scif.img.ImgOpener
import java.awt.image.BufferedImage
import net.imglib2.view.Views
import java.awt.Color
import net.imglib2.`type`.numeric.RealType
import scala.collection.JavaConverters._
import java.io.File
import javax.imageio.ImageIO
import scala.Left
import scala.Right

object LearnImageUtil {

  def attributeListToImageStack(attributeList: AttributeList): Option[ImageStack] = {
    val width = attributeList.get(TagFromName.Columns).getIntegerValues()(0)
    val height = attributeList.get(TagFromName.Rows).getIntegerValues()(0)
    val pixels = attributeList.getPixelData.getShortValues
    val ip = new ShortProcessor(width, height, pixels, null)
    val img = NewImage.createShortImage("hey", width, height, 1, 0)
    val stack: ImageStack = new ImageStack(width, height);
    stack.addSlice("hey", ip);
    Some(stack)
  }

}