package edu.umro.ImageUtil2.copy;

import java.util.TreeMap
import net.imglib2.Cursor
import net.imglib2.Interval
import net.imglib2.RandomAccessibleInterval
import net.imglib2.algorithm.neighborhood.Neighborhood
import net.imglib2.algorithm.neighborhood.RectangleShape
import net.imglib2.`type`.NativeType
import net.imglib2.`type`.numeric.RealType
import net.imglib2.util.Intervals
import net.imglib2.view.Views
import scala.collection.JavaConverters._
import net.imglib2.RandomAccess

/**
 * Find potentially bad pixels in an image.
 */
object FindWorstPixels2 {

  type TT = NativeType[_] with RealType[_]

  /**
   * Subtracts each pixel from its neighbors, averages the absolute value of the
   * differences, and returns a list of the worst (most different) pixels.
   *
   * @param source
   *            - the image data to work on
   * @param count
   *            - the number of pixels to return
   * @return - a list of the worst pixels, sorted with the worst (highest value)
   *         last
   */

  def findWorstPixels[T](source: RandomAccessibleInterval[T], fcount: Int) {

    val interval = Intervals.expand(source, -1)

    // create a view on the source with this interval
    val intervalSource = Views.interval(source, interval)

    // create a Cursor that iterates over the source and checks in a 8-neighborhood
    // for how different it is
    val center = Views.iterable(intervalSource).cursor.asInstanceOf[Cursor[TT]]

    val ra = source.randomAccess.asInstanceOf[RandomAccess[TT]]

    var count = 0
    while (center.hasNext) {
      val centerValue = center.next
      val x = center.getIntPosition(0)
      val y = center.getIntPosition(1)
      val v = centerValue.getRealFloat
      ra.setPosition(Array(x, y))
      
      val vv = ra.get.getRealFloat

      count = count + 1
    }

    println("count: " + count)

    // instantiate a RectangleShape to access rectangular local neighborhoods
    // of radius 1 (that is 3x3x...x3 neighborhoods), skipping the center pixel
    // (this corresponds to an 8-neighborhood in 2d or 26-neighborhood in 3d, ...)
    val shape = new RectangleShape(1, true);

//    val mostExtreme = new TreeMap[Float, Cursor[T]]();
//
//    val locNeigh = shape.neighborhoods(intervalSource).cursor.asScala.asInstanceOf[Iterator[T]]
//
//    def getDiff(any: Any) = {
//      val cursor = any.asInstanceOf[java.util.Iterator[Object]]
//      val cc = cursor.asScala.asInstanceOf[Iterator[T]]
//      println("cc.size: " + cc.size)
//    }

    //println("locNeigh.size: " + locNeigh.size)

    // iterate over the set of neighborhoods in the image

//    val jj: java.util.Iterator[Neighborhood[T]] = shape.neighborhoods(intervalSource).iterator()
//
//    locNeigh.map(n => getDiff(n.asInstanceOf[Any]))

    println("hey")

    //		for ( Neighborhood<T> localNeighborhood : shape.neighborhoods(intervalSource)) {
    //			// what is the value that we investigate?
    //			// (the center cursor runs over the image in the same iteration order as
    //			// neighborhood)
    //			val centerValue = center.next();
    //			val cv: Double = centerValue.getRealFloat();
    //
    //			float total = 0;
    //
    //			// sum the absolute value of the differences from adjancent pixels
    //			for (final T value : localNeighborhood) {
    //				// find difference
    //				double v = value.getRealFloat();
    //				total += Math.abs(v - cv);
    //			}
    //
    //			float avg = total / localNeighborhood.size();
    //
    //			mostExtreme.put(avg, center.copyCursor());
    //
    //			while (mostExtreme.size() > count)
    //				mostExtreme.remove(mostExtreme.firstKey());
    //		}
    //
    //		return mostExtreme;
    ???
  }

  def main(args: Array[String]): Unit = {

    val fileName = "src\\test\\resources\\TestFindWorstPixels.dcm"

    val imgPlus = ImageUtil.readDicomFile(fileName).right.get

    val worst = findWorstPixels(imgPlus, 10)

    println("worst: " + worst)
  }
}