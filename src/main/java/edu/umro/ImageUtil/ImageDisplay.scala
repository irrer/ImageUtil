package edu.umro.ImageUtil

import java.awt.image.BufferedImage
import java.awt.Desktop
import java.awt.Dimension
import java.awt.Graphics
import java.io.File
import java.net.URI
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

  /**
    * Create a chart in a temporary file and display it with a browser.
    *
    * Note that you must wait for the page to be displayed before terminating the program.
    * @param data List of data points.
    * @param xName X axis name, defaults to X
    * @param yName Y axis name, defaults to Y
    * @param title Name of chart, defaults to "Line Chart"
    */
  def showChart(data: Seq[Double], xName: String = "X", yName: String = "Y", title: String = "Line Chart"): Unit = {

    val xText = data.indices.mkString(",")
    val yText = data.mkString(",")

    val htmlText =
      s"""
         |<!DOCTYPE html>
         |<html lang="en">
         |<head>
         |    <meta charset="UTF-8">
         |    <title>C3.js Line Chart Example</title>
         |    <!-- C3.css -->
         |    <link href="https://cdnjs.cloudflare.com/ajax/libs/c3/0.7.20/c3.min.css" rel="stylesheet">
         |    <style>
         |        body {
         |            font-family: Arial, sans-serif;
         |            margin: 2em;
         |        }
         |        #chart {
         |            max-width: 800px;
         |            margin: auto;
         |        }
         |    </style>
         |</head>
         |<body>
         |    <h2>$title</h2>
         |    <div id="chart"></div>
         |
         |    <script src="https://cdnjs.cloudflare.com/ajax/libs/d3/5.16.0/d3.min.js"></script>
         |    <script src="https://cdnjs.cloudflare.com/ajax/libs/c3/0.7.20/c3.min.js"></script>
         |    <script>
         |        // Generate 100 simple random data points
         |        // Example: y-values are random, x-values are 1 to 100
         |        var xData = [$xText],
         |            yData = ['$yName', $yText];
         |        var chart = c3.generate({
         |            bindto: '#chart',
         |            data: {
         |                x: 'x',
         |                columns: [
         |                    ['x'].concat(xData),
         |                    yData
         |                ],
         |                type: 'line'
         |            },
         |            axis: {
         |                x: {
         |                    label: 'X Value',
         |                    tick: {
         |                        count: 10
         |                    }
         |                },
         |                y: {
         |                    label: 'Y Value'
         |                }
         |            }
         |        });
         |    </script>
         |</body>
         |</html>
         |
         |""".stripMargin

    val tempFile = File.createTempFile("tempChart", ".html")
    tempFile.deleteOnExit()
    edu.umro.ScalaUtil.FileUtil.writeFile(tempFile, htmlText)

    val fileUri: URI = tempFile.toURI
    Desktop.getDesktop.browse(fileUri)
  }

  def main(args: Array[String]): Unit = {

    if (true) {
      showChart(Seq(1.1, 2.2, 4.4, 3.3))
      Thread.sleep(5 * 1000)
    }

    if (false) {
      val image = new BufferedImage(256, 346, BufferedImage.TYPE_INT_RGB)
      // put a pattern in the image
      (0 until image.getWidth).foreach(x => (0 until image.getHeight).foreach(y => image.setRGB(x, y, (x * y * 11) % 0xffffff)))
      showInMSPaint(image)
      Thread.sleep(100 * 1000)
    }
  }
}
