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

class TestVector {
    @Test
    def testConstructor() = {
        val x = 1.0
        val y = 2.0
        val z = 3.0

        val v = new Vector(x, y, z)

        assertEquals(x, v(0), 0.0)
        assertEquals(y, v(1), 0.0)
        assertEquals(z, v(2), 0.0)
    }
    
    @Test
    def testEquals() = {
        val u = new Vector(1.0, 2.0, 3.0)
        val v = new Vector(1.0, 2.0, 3.0)
        
        assertEquals(u, v)
    }
    
    @Test
    def testToString() = {
        val v = new Vector(1.0, 2.0, 3.0)
        val s = v.toString()
        
        assertEquals("Vector [1.0, 2.0, 3.0]", s)
    }
    
    @Test
    def testMap2() = {
        val u = new Vector(2.0, 3.0, 4.0);
        val v = new Vector(3.0, 4.0, 5.0);
        
        val w = u.map2(v)((x, y) => x + y);
        
        w match {
            case Right(x) => assertEquals(new Vector(5.0, 7.0, 9.0), x)
            case Left(error) => fail()
        }
    }
    
    @Test
    def testMap2WrongSize() = {
        val u = new Vector(2.0, 3.0, 4.0);
        val v = new Vector(3.0, 4.0);
        
        val w = u.map2(v)((x, y) => x + y);
        
        val expectedError = "Vector operation performed mismatching vector length. First length: 3. Second length: 2"
        
        w match {
            case Right(x) => fail()
            case Left(exception) => assertEquals(expectedError, exception.getMessage())
        }
    }
    
    @Test
    def testMap() = {
        val u = new Vector(2.0, 3.0)
        val v = u.map(x => x * x)
        
        assertEquals(new Vector(4.0, 9.0), v)
    }
    
    @Test
    def testReduce() = {
        val v = new Vector(1.0, 2.0, 3.0)
        
        val r = v.reduce((x, y) => x + y)
        
        assertEquals(6.0, r, 0.0)
    }
    
    @Test
    def testDot() = {
        val v = new Vector(1.0, 2.0, 3.0)
        
        val result = v.dot(v)
        
        result match {
            case Right(d) => assertEquals(14.0, d, 0.0)
            case Left(error) => fail(error.getMessage())
        }   
    }
    
    @Test
    def testMagnitude() = {
        val v = new Vector(3.0, 4.0)
        
        val magnitude = v.magnitude
        
        assertEquals(5.0, magnitude, 0.0)
    }
    
    @Test
    def testUnitize() = {
        val v = new Vector(4.0, 5.0)
        
        val u = v.unitize()
        
        assertEquals(0.625, u(0), 1e-3)
        assertEquals(0.781, u(1), 1e-3)
    }
    
    @Test
    def testUnitizeZeros() = {
        val v = new Vector(0.0, 0.0)
        
        val u = v.unitize()
        
        assertTrue(u(0).isNaN)
        assertTrue(u(1).isNaN)
    }
    
    @Test
    def testHashCodeDifferent() = {
        val u = new Vector(1.0, 2.0)
        val v = new Vector(2.0, 3.0)
        
        assertNotEquals(u.hashCode(), v.hashCode())
    }
    
    @Test
    def testHashCodeSame() = {
        val u = new Vector(1.0, 2.0)
        val v = new Vector(1.0, 2.0)
        
        assertEquals(u.hashCode(), v.hashCode())
    }
    
    @Test
    def testPlus() = {
        val u = new Vector(1.0, 2.0)
        val v = new Vector(3.0, 4.0)
        
        val result = u + v
        
        result match {
            case Left(exception) => fail()
            case Right(sum) => assertEquals(new Vector(4.0, 6.0), sum)
        }
    }
    
    @Test
    def testPlusWrongSize() = {
        testOperationOnWrongSizeVectors((a: Vector, b: Vector) => a + b)
    }
    
    @Test
    def testMinus() = {
        val u = new Vector(1.0, 2.0)
        val v = new Vector(3.0, 4.0)
        
        val result = v - u
        
        result match {
            case Left(exception) => fail()
            case Right(sum) => assertEquals(new Vector(2.0, 2.0), sum)
        }
    }
    
    @Test
    def testMinusWrongSize() = {
        testOperationOnWrongSizeVectors((a: Vector, b: Vector) => a - b)
    }
    
    @Test
    def testVectorMultiply() = {
        val u = new Vector(1.0, 2.0)
        val v = new Vector(3.0, 4.0)
        
        val result = v * u
        
        result match {
            case Left(exception) => fail()
            case Right(sum) => assertEquals(new Vector(3.0, 8.0), sum)
        }
    }
    
    @Test
    def testVectorMultiplyWrongSize() = {
        testOperationOnWrongSizeVectors((a: Vector, b: Vector) => a * b)
    }
    
    def testOperationOnWrongSizeVectors(f: (Vector, Vector) => Either[Exception,Vector]) = {
        val u = new Vector(2.0, 3.0, 4.0);
        val v = new Vector(3.0, 4.0);
        
        val w = f(u, v)
        
        val expectedError = "Vector operation performed mismatching vector length. First length: 3. Second length: 2"
        
        w match {
            case Right(x) => fail()
            case Left(exception) => assertEquals(expectedError, exception.getMessage())
        }
    }
    
    @Test
    def testScalarMultiply() = {
        val v = new Vector(2.0, 3.0)
        
        val w = v * 3.0
        
        assertEquals(new Vector(6.0, 9.0), w)
    }
    
    @Test
    def testScalarDivide() = {
        val v = new Vector(4.0, 8.0)
        
        val w = v / 2.0
        
        assertEquals(new Vector(2.0, 4.0), w)
    }
}