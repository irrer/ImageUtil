package edu.umro.ImageUtil.test;

import java.io.File;
import java.util.TreeMap;
import edu.umro.ImageUtil.FindWorstPixels;
import ij.ImageJ;
import io.scif.img.ImgIOException;
import io.scif.img.ImgOpener;
import io.scif.img.SCIFIOImgPlus;
import net.imglib2.Cursor;
import net.imglib2.algorithm.region.hypersphere.HyperSphere;
import net.imglib2.exception.IncompatibleTypeException;
import net.imglib2.img.display.imagej.ImageJFunctions;
import net.imglib2.type.NativeType;
import net.imglib2.type.numeric.RealType;

/**
 * Find the pixels whose value is most different from their neighbors and
 * display them with spheres in another image.
 *
 * Based on <code>Example4b</code>.
 */
public class TestFindWorstPixels {

	public <T extends RealType<T> & NativeType<T>> void testFindWorstPixels(String fileName) throws ImgIOException {
		// open with ImgOpener
		@SuppressWarnings("unchecked")
		SCIFIOImgPlus<T> img = (SCIFIOImgPlus<T>) (new ImgOpener().openImgs(fileName).get(0));

		TreeMap<Float, Cursor<T>> worstList = FindWorstPixels.findWorstPixels(img, 6);

		boolean onOff = false;
		System.out.println("list of the worst pixels");

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
		String fileName = "src\\test\\resources\\TestFindWorstPixels.dcm";
		if (args.length > 0) {
			fileName = args[0];
		}
		System.out.println("Using file: " + (new File(fileName)));
		(new TestFindWorstPixels()).testFindWorstPixels(fileName);
	}
}