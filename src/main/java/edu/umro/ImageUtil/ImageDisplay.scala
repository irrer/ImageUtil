package edu.umro.ImageUtil

import java.awt.image.BufferedImage
import java.awt.Dimension
import java.awt.Graphics
import java.io.File
import java.util.concurrent.CountDownLatch
import javax.imageio.ImageIO
import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.WindowConstants

/**
  * Utilities for displaying an image to the user.  Usually these are used for debugging.
  */
object ImageDisplay {

  /**
    * Given an image, display it to the user in a new window.  Do not return until the user closes the window.
    * @param img Show this image.
    */
  //noinspection ScalaUnusedSymbol
  def showImageAndWait(img: BufferedImage): Unit = {
    val latch = new CountDownLatch(1)

    // Create a JPanel that paints the image
    val panel = new JPanel() {
      override def paintComponent(g: Graphics): Unit = {
        super.paintComponent(g)
        g.drawImage(img, 0, 0, this)
      }

      override def getPreferredSize: Dimension =
        new Dimension(img.getWidth, img.getHeight)
    }

    // Create and set up the window
    val frame = new JFrame("Image Viewer")
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    frame.add(panel)
    frame.pack()
    frame.setLocationRelativeTo(null) // Center on screen
    frame.setVisible(true)

    // Add a window listener to release the latch when closed
    frame.addWindowListener(new java.awt.event.WindowAdapter {
      override def windowClosed(e: java.awt.event.WindowEvent): Unit = {
        latch.countDown()
      }
    })

    // Wait until window is closed
    latch.await()
  }

  /**
    * Show the given image using MS Paint.  Image is first written to a temporary file which
    * is deleted when the calling program exits.
    *
    * @param img Image to be shown.
    */
  //noinspection ScalaUnusedSymbol
  //noinspection ScalaWeakerAccess
  def showInMSPaint(img: BufferedImage): Unit = {
    // Save the image to a temporary PNG file
    val tempFile = File.createTempFile("tempImage", ".png")
    tempFile.deleteOnExit()
    ImageIO.write(img, "png", tempFile)

    // Command to open MS Paint with the temp file
    val paintCmd = Array("mspaint", tempFile.getAbsolutePath)
    // Start the process
    val process = new ProcessBuilder(paintCmd: _*).start()
    // Optionally wait for Paint to open (not necessary here)
  }

  def main(args: Array[String]): Unit = {
    val image = new BufferedImage(256, 346, BufferedImage.TYPE_INT_RGB)
    // put a pattern in the image
    (0 until image.getWidth).foreach(x => (0 until image.getHeight).foreach(y => image.setRGB(x, y, (x * y * 11) % 0xffffff)))
    showInMSPaint(image)
    Thread.sleep(100 * 1000)
  }
}
