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

package edu.umro.ImageUtil.test

import edu.umro.ImageUtil.DicomImage
import org.scalatest.{FlatSpec, Matchers}

class TestDicomImageSumHistogram extends FlatSpec with Matchers {

  "sum of histograms" should "superset" in {
    def hp(v: Float, c: Int) = DicomImage.HistPoint(v, c)
    val h1 = Seq(hp(1, 5), hp(3, 4), hp(5, 1))
    val h2 = Seq(hp(1, 2), hp(2, 5), hp(6, 1))
    val h3 = Seq(hp(1, 2), hp(2, 5), hp(6, 1), hp(7, 0))

    val expected = Seq(hp(1, 9), hp(2, 10), hp(3, 4), hp(5, 1), hp(6, 2))

    val actual = DicomImage.histogramSum(Seq(h1, h2, h3))

    actual.size should be(expected.size)

    actual should be(expected)
  }

}
