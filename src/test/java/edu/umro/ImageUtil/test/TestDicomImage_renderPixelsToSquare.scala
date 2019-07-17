package edu.umro.ImageUtil.test;

import com.pixelmed.dicom.AttributeList
import java.io.File
import edu.umro.ImageUtil.DicomImage
import java.awt.Rectangle
import javax.swing.JFrame
import java.awt.FlowLayout
import javax.swing.JLabel
import javax.swing.ImageIcon
import java.awt.Color

/**
 * Check the function that corrects for aspect ratio.
 */
object TestDicomImage_renderPixelsToSquare {

  private def show(img: DicomImage) = {

    class Runny extends Runnable {
      def run = {
        val bufImg = img.toBufferedImage(Color.green)

        val frame = new JFrame
        frame.getContentPane.setLayout(new FlowLayout)
        frame.getContentPane().add(new JLabel(new ImageIcon(bufImg)))
        frame.pack
        frame.setLocationRelativeTo(null)
        frame.setVisible(true)
      }
    }

    val runny = new Runny
    val thread = new Thread(runny)
    thread.start
  }

  def main(args: Array[String]): Unit = {

    val size = 100
    val light = 1000.toFloat
    val dark = 0.toFloat
    val pixels = (0 until size).map(y => (0 until size).map(x => if ((x - y).abs < 5) light else dark))
    val dicomImage = new DicomImage(pixels)

    //val noop = dicomImage.renderPixelsToSquare(2, 2)
    //show(noop)

    val taller = dicomImage.renderPixelsToSquare(1, 2.5)
    show(taller)

    // val wider = dicomImage.renderPixelsToSquare(2, 1)

    Thread.sleep(5 * 1000)
    println("TestDicomImage_renderPixelsToSquare done")

  }

}