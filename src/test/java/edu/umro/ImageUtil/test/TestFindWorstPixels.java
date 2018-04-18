package edu.umro.ImageUtil.test;

import java.util.TreeMap;

import edu.umro.ImageUtil.FindWorstPixels;
import ij.ImageJ;
import io.scif.img.ImgIOException;
import io.scif.img.ImgOpener;
import io.scif.img.SCIFIOImgPlus;
import net.imglib2.Cursor;
import net.imglib2.algorithm.region.hypersphere.HyperSphere;
import net.imglib2.exception.IncompatibleTypeException;
import net.imglib2.img.Img;
import net.imglib2.img.array.ArrayImgFactory;
import net.imglib2.img.display.imagej.ImageJFunctions;
import net.imglib2.type.NativeType;
import net.imglib2.type.logic.BitType;
import net.imglib2.type.numeric.RealType;

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

		TreeMap<Float, Cursor<T>> worst = FindWorstPixels.findWorstPixels(img, 2);

		boolean onOff = false;

		for (float bad : worst.keySet()) {
			Cursor<T> cursor = worst.get(bad);
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