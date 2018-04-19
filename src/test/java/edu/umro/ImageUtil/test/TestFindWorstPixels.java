package edu.umro.ImageUtil.test;

import java.util.TreeMap;
import java.util.function.Consumer;

import edu.umro.ImageUtil.FindWorstPixels;
import ij.ImageJ;
import io.scif.img.ImgIOException;
import io.scif.img.ImgOpener;
import io.scif.img.SCIFIOImgPlus;
import net.imglib2.Cursor;
import net.imglib2.FinalInterval;
import net.imglib2.Interval;
import net.imglib2.RandomAccessibleInterval;
import net.imglib2.algorithm.region.hypersphere.HyperSphere;
import net.imglib2.exception.IncompatibleTypeException;
import net.imglib2.img.display.imagej.ImageJFunctions;
import net.imglib2.type.NativeType;
import net.imglib2.type.numeric.RealType;
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

		@SuppressWarnings("unchecked")
		SCIFIOImgPlus<T> img = (SCIFIOImgPlus<T>) (new ImgOpener().openImgs(fileName).get(0));

		TreeMap<Float, Cursor<T>> worstList = FindWorstPixels.findWorstPixels(img, 5);

		if (System.out != null) {
			FinalInterval j = Intervals.createMinSize(10, 10, 20, 20);
			final RandomAccessibleInterval<T> ivs = Views.interval(img, j);
			final Cursor<T> curse = Views.iterable(ivs).cursor();

			class Foo implements Consumer<T> {

				int minX = 1000000;
				int maxX = -1;
				int minY = 1000000;
				int maxY = -1;
				long count = 0;

				@Override
				public void accept(T t) {
					int x = curse.getIntPosition(0);
					int y = curse.getIntPosition(1);
					if (x < minX)
						minX = x;
					if (x > maxX)
						maxX = x;
					if (y < minY)
						minY = y;
					if (y > maxY)
						maxY = y;
					count++;
					if (((x + y) % 2) == 0)
						t.setReal(50000);
					else
						t.setZero();
				}

			}

			Foo foo = new Foo();
			Consumer<? super T> action = foo;
			curse.forEachRemaining(action);
			System.out.println("minX: " + foo.minX + "    maxX: " + foo.maxX);
			System.out.println("minY: " + foo.minY + "    maxY: " + foo.maxY);
			System.out.println("count: " + foo.count);

		}

		boolean onOff = false;

		for (float bad : worstList.keySet()) {
			Cursor<T> cursor = worstList.get(bad);
			System.out.println("    " + bad + " : " + cursor.getIntPosition(0) + ", " + cursor.getIntPosition(1));
			HyperSphere<T> inner = new HyperSphere<T>(img, cursor, 6);
			HyperSphere<T> outer = new HyperSphere<T>(img, cursor, 9);

			for (T out : outer) {
				boolean markIt = true;
				for (T in : inner) {
					if (in.compareTo(out) == 0) {
						markIt = false;
						break;
					}
				}
				if (markIt) {
					if (onOff) {
						out.setReal(50000);
					} else {
						out.setReal(0);
					}
					onOff = !onOff;
				}
			}
		}

		// display input
		ImageJFunctions.show(img);
	}

	public static void main(String[] args) throws ImgIOException, IncompatibleTypeException {
		// open an ImageJ window
		new ImageJ();

		(new TestFindWorstPixels()).testFindWorstPixels();
	}
}