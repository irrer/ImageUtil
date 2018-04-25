package edu.umro.ImgUtilities;

import java.util.TreeMap;

import io.scif.img.SCIFIOImgPlus;
import net.imglib2.Cursor;
import net.imglib2.Interval;
import net.imglib2.RandomAccessibleInterval;
import net.imglib2.algorithm.neighborhood.Neighborhood;
import net.imglib2.algorithm.neighborhood.RectangleShape;
import net.imglib2.type.NativeType;
import net.imglib2.type.numeric.RealType;
import net.imglib2.util.Intervals;
import net.imglib2.view.Views;

/**
 * Find potentially bad pixels in an image.
 */
public class FindWorstPixels {

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
//	public static <T extends RealType<T> & NativeType<T>> TreeMap<Float, Cursor<T>> findWorstPixels(
//			final RandomAccessibleInterval<T> source, final int count) {

	public static <T extends RealType<T> & NativeType<T>> TreeMap<Float, Cursor<T>> findWorstPixels(
			final RandomAccessibleInterval<T> source, final int count) {

		Interval interval = Intervals.expand(source, -1);

		// create a view on the source with this interval
		final RandomAccessibleInterval<T> intervalSource = Views.interval(source, interval);

		// create a Cursor that iterates over the source and checks in a 8-neighborhood
		// for how different it is
		final Cursor<T> center = Views.iterable(intervalSource).cursor();

		// instantiate a RectangleShape to access rectangular local neighborhoods
		// of radius 1 (that is 3x3x...x3 neighborhoods), skipping the center pixel
		// (this corresponds to an 8-neighborhood in 2d or 26-neighborhood in 3d, ...)
		final RectangleShape shape = new RectangleShape(1, true);

		TreeMap<Float, Cursor<T>> mostExtreme = new TreeMap<Float, Cursor<T>>();

		// iterate over the set of neighborhoods in the image
		for (final Neighborhood<T> localNeighborhood : shape.neighborhoods(intervalSource)) {
			// what is the value that we investigate?
			// (the center cursor runs over the image in the same iteration order as
			// neighborhood)
			final T centerValue = center.next();
			double cv = centerValue.getRealFloat();

			float total = 0;

			// sum the absolute value of the differences from adjacent pixels
			for (final T value : localNeighborhood) {
				// find difference
				double v = value.getRealFloat();
				total += Math.abs(v - cv);
			}

			float avg = total / localNeighborhood.size();

			mostExtreme.put(avg, center.copyCursor());

			while (mostExtreme.size() > count)
				mostExtreme.remove(mostExtreme.firstKey());
		}

		return mostExtreme;
	}

}