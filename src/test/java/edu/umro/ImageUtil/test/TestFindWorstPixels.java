package edu.umro.ImageUtil.test;

import java.util.TreeMap;

import edu.umro.ImageUtil.FindWorstPixels;
import ij.ImageJ;
import io.scif.img.ImgIOException;
import io.scif.img.ImgOpener;
import io.scif.img.SCIFIOImgPlus;
import net.imglib2.Cursor;
import net.imglib2.Interval;
import net.imglib2.RandomAccessibleInterval;
import net.imglib2.algorithm.neighborhood.Neighborhood;
import net.imglib2.algorithm.neighborhood.RectangleShape;
import net.imglib2.algorithm.region.hypersphere.HyperSphere;
import net.imglib2.exception.IncompatibleTypeException;
import net.imglib2.img.Img;
import net.imglib2.img.ImgFactory;
import net.imglib2.img.array.ArrayImgFactory;
import net.imglib2.img.display.imagej.ImageJFunctions;
import net.imglib2.type.NativeType;
import net.imglib2.type.logic.BitType;
import net.imglib2.type.numeric.RealType;
import net.imglib2.type.numeric.integer.UnsignedShortType;
import net.imglib2.util.Intervals;
import net.imglib2.view.Views;

/**
 * Find the pixels whose value is most different from their neighbors and
 * display them with spheres in another image.
 *
 * Based on <code>Example4b</code>.
 */
public class TestFindWorstPixels {
	public <T extends RealType<T> & NativeType<T>> void testFindWorstPixels() throws ImgIOException {
		// open with ImgOpener
		String fileName = "src\\test\\resources\\TestFindWorstPixels.dcm";

		// Img<T> img = (Img<T>) new ImgOpener().openImg(fileName);
		SCIFIOImgPlus<T> img = (SCIFIOImgPlus<T>) (new ImgOpener().openImgs(fileName).get(0));

		TreeMap<Float, Cursor<T>> worst = FindWorstPixels.findWorstPixels(img, 20);

		for (float bad : worst.keySet()) {
			Cursor<T> cursor = worst.get(bad);
			System.out.println("    " + bad + " : " + cursor.getIntPosition(0) + ", " + cursor.getIntPosition(1));
		}

		// display output and input
		ImageJFunctions.show(img);
		// ImageJFunctions.show(display);
	}

	public static void main(String[] args) throws ImgIOException, IncompatibleTypeException {
		// open an ImageJ window
		new ImageJ();

		(new TestFindWorstPixels()).testFindWorstPixels();
	}
}