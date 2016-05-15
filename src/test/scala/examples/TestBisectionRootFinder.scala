/*
 * Copyright 2015 Jason Lenthe
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package examples

import org.junit._
import org.junit.Assert._

class TestBisectionRootFinder {
  @Test
  def testApplySuccess() {
    val rf = new BisectionRootFinder(1e-6);
    val result = rf(x => x*x*x - 27.0, 0.0, 11.0)
    result match {
      case Right(r) => assertEquals(3.0, r, 1e-6)
      case Left(errorMessage) => fail(errorMessage)
    }
    
  }
  
  @Test
  def testApplyReversedBracket() {
    val rf = new BisectionRootFinder(1e-6);
    val result = rf(x => x*x*x - 27.0, 11.0, 1.0)
    result match {
      case Right(r) => assertEquals(3.0, r, 1e-6)
      case Left(errorMessage) => fail(errorMessage)
    }
    
  }
  
  @Test
  def testApplyNotBracketed() {
    val rf = new BisectionRootFinder(1e-6);
    val result = rf(x => x*x*x - 27.0, 5.0, 11.0)
    result match {
      case Right(r) => fail(s"Should have gotten an error. The value returned was ${r}.")
      case Left(errorMessage) => assertEquals("The input function points did not bracket the root.  First point (a, fa) = (5.0, 98.0).  Second point (b, fb) = (11.0, 1304.0).", errorMessage)
    }
  }
}