package learn;

import java.util.function.Consumer;

import ij.ImageJ;
import io.scif.img.ImgIOException;
import io.scif.img.ImgOpener;
import io.scif.img.SCIFIOImgPlus;
import net.imglib2.Cursor;
import net.imglib2.FinalInterval;
import net.imglib2.RandomAccessibleInterval;
import net.imglib2.exception.IncompatibleTypeException;
import net.imglib2.img.display.imagej.ImageJFunctions;
import net.imglib2.type.NativeType;
import net.imglib2.type.numeric.RealType;
import net.imglib2.util.Intervals;
import net.imglib2.view.Views;

public class ImToPng {

	public <T extends RealType<T> & NativeType<T>> void doStuff() throws ImgIOException {

		String fileName = "src\\test\\resources\\TestFindWorstPixels.dcm";

		@SuppressWarnings("unchecked")
		SCIFIOImgPlus<T> img = (SCIFIOImgPlus<T>) (new ImgOpener().openImgs(fileName).get(0));

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

	public <T extends RealType<T> & NativeType<T>> void testFindWorstPixels() throws ImgIOException {
		// open with ImgOpener
		String fileName1 = "src\\test\\resources\\TestFindWorstPixels.dcm";
		String fileName2 = "src\\test\\resources\\banner.png";

		@SuppressWarnings("unchecked")
		SCIFIOImgPlus<T> img1 = (SCIFIOImgPlus<T>) (new ImgOpener().openImgs(fileName1).get(0));
		
		@SuppressWarnings("unchecked")
		SCIFIOImgPlus<T> img2 = (SCIFIOImgPlus<T>) (new ImgOpener().openImgs(fileName2).get(0));

		int pixelType1 = img1.getImageMetadata().getPixelType();
		System.out.println("pixelType1: " + pixelType1);

		int pixelType2 = img2.getImageMetadata().getPixelType();
		System.out.println("pixelType2: " + pixelType2);

		ImageJFunctions.show(img1);
		ImageJFunctions.show(img2);
	}

	public static void main(String[] args) throws ImgIOException, IncompatibleTypeException {
		// open an ImageJ window
		new ImageJ();

		(new ImToPng()).testFindWorstPixels();
	}
}