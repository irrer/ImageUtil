/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * Open Source Physics software is free software as described near the bottom of this code file.
 *
 * For additional information and documentation on Open Source Physics please see: 
 * <http://www.opensourcephysics.org/>
 */

package org.opensourcephysics.numerics;

public class CubicSpline implements Function {

	/**
	 * First derivative at first point.
	 */
	private double startDerivative = Double.NaN;

	/**
	 * First derivative at last point.
	 */
	private double endDerivative = Double.NaN;
	private int guessIndex = 1;

	/** Data */
	double[] xd, yd;

	/** Interpolating coefficients */
	double[] coefficients;

	/** Flag for x arra ordering: 1 is decreasing; 1 is increasing; 0 is unsorted */
	int sign;

	/**
	 * Constructs a CubicSpline interpolating function from the given data.
	 *
	 * X data array must be sorted in increasing or decreasing value. The
	 * checkSorting method in the Util package can be used to check the sorting of
	 * an array.
	 *
	 * @param xdata
	 *            double[]
	 * @param ydata
	 *            double[]
	 */
	public CubicSpline(double[] xdata, double[] ydata) {
		update(xdata, ydata);
	}

	/**
	 * Checks if an array is sorted. Returns: Positive integer if array is sorted in
	 * increasing value. Negative integer if array is sorted in decreasing value.
	 * Zero if array is not sorted.
	 *
	 * @param array
	 *            double[]
	 * @return int 1,0,-1 based on sorting
	 */
	private static int checkSorting(double[] array) {
		int sign = (array[0] <= array[array.length - 1]) ? 1 : -1;
		for (int i = 1, n = array.length; i < n; i++) {
			switch (sign) {
			case -1:
				if (array[i - 1] < array[i]) {
					return 0; // not sorted
				}
				break;
			case 1:
				if (array[i - 1] > array[i]) {
					return 0; // not sorted
				}
			}
		}
		return sign;
	}

	public void update(double[] xdata, double[] ydata) {
		if (xd == null || xd.length != xdata.length) {
			xd = (double[]) xdata.clone();
		} else {
			System.arraycopy(xdata, 0, xd, 0, xd.length);
		}
		if (yd == null || yd.length != ydata.length) {
			yd = (double[]) ydata.clone();
		} else {
			System.arraycopy(ydata, 0, yd, 0, yd.length);
		}
		if (xd.length != yd.length) {
			throw new IllegalArgumentException("Arrays must be of equal length.");
		}
		sign = checkSorting(xd);
		if (sign == 0) {
			throw new IllegalArgumentException("X array must be sorted in either increasing or decreasing order.");
		}
		if (xd.length > 2) {
			computeSecondDerivatives();
		}
	}

	/**
	 * Computes the interpolated y value for a given x value.
	 *
	 * @param x
	 * @return interpolated y value.
	 */
	public double evaluate(double x) {
		int n1 = 0;
		int n2 = xd.length - 1;
		if (n2 < 1) {
			return yd[0]; // only one data point.
		}
		if (n2 == 1) {
			return Interpolation.linear(x, xd[0], xd[1], yd[0], yd[1]); // only two data points
		}
		if (sign * x < sign * xd[1]) { // use first spline
			n2 = 1;
		} else if (sign * x > sign * xd[n2 - 1]) { // use last spline
			n1 = n2 - 1;
		} else { // check if we are close to the guessed index
			if (guessIndex > 0 && sign * x > sign * xd[guessIndex - 1]) {
				n1 = guessIndex - 1;
			}
			if (guessIndex < n2 && sign * x < sign * xd[guessIndex + 1]) {
				n2 = guessIndex + 1;
			}
		}
		while (n2 - n1 > 1) {
			int n = (n1 + n2) / 2;
			if (sign * xd[n] > sign * x) {
				n2 = n;
			} else {
				n1 = n;
			}
		}
		guessIndex = n1; // the next x value is often close to the current value so save this index
		double step = xd[n2] - xd[n1];
		double a = (xd[n2] - x) / step;
		double b = (x - xd[n1]) / step;
		return a * yd[n1] + b * yd[n2]
				+ (a * (a * a - 1) * coefficients[n1] + b * (b * b - 1) * coefficients[n2]) * step * step / 6;
	}

	private void computeSecondDerivatives() {
		int n = xd.length;
		double w, s;
		double[] u = new double[n - 1];
		coefficients = new double[n];
		if (Double.isNaN(startDerivative)) {
			coefficients[0] = u[0] = 0;
		} else {
			coefficients[0] = -0.5;
			u[0] = 3.0 / (xd[1] - xd[0]) * ((yd[1] - yd[0]) / (xd[1] - xd[0]) - startDerivative);
		}
		for (int i = 1; i < n - 1; i++) {
			double invStep2 = 1 / (xd[i + 1] - xd[i - 1]);
			s = (xd[i] - xd[i - 1]) * invStep2;
			w = 1 / (s * coefficients[i - 1] + 2);
			coefficients[i] = (s - 1) * w;
			u[i] = (6 * invStep2
					* ((yd[i + 1] - yd[i]) / (xd[i + 1] - xd[i]) - (yd[i] - yd[i - 1]) / (xd[i] - xd[i - 1]))
					- s * u[i - 1]) * w;
		}
		if (Double.isNaN(endDerivative)) {
			w = s = 0;
		} else {
			w = -0.5;
			s = 3.0 / (xd[n - 1] - xd[n - 2]) * (endDerivative - (yd[n - 1] - yd[n - 2]) / (xd[n - 1] - xd[n - 2]));
		}
		coefficients[n - 1] = (s - w * u[n - 2]) / (w * coefficients[n - 2] + 1);
		for (int i = n - 2; i >= 0; i--) {
			coefficients[i] = coefficients[i] * coefficients[i + 1] + u[i];
		}
		return;
	}
}

/*
 * Open Source Physics software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL) as
 * published by the Free Software Foundation; either version 2 of the License,
 * or(at your option) any later version.
 * 
 * Code that uses any portion of the code in the org.opensourcephysics package
 * or any subpackage (subdirectory) of this package must must also be be
 * released under the GNU GPL license.
 *
 * This software is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this; if not, write to the Free Software Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston MA 02111-1307 USA or view the license online at
 * http://www.gnu.org/copyleft/gpl.html
 *
 * Copyright (c) 2007 The Open Source Physics project
 * http://www.opensourcephysics.org
 */
