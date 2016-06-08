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

import java.lang.IllegalArgumentException

import org.junit._
import org.junit.Assert._

class TestVector {
    @Test
    def testConstructor() = {
        val x = 1.0
        val y = 2.0
        val z = 3.0

        val v = Vector(x, y, z)

        assertEquals(x, v(0), 0.0)
        assertEquals(y, v(1), 0.0)
        assertEquals(z, v(2), 0.0)
    }
    
    @Test
    def testEquals() = {
        val u = Vector(1.0, 2.0, 3.0)
        val v = Vector(1.0, 2.0, 3.0)
        assertEquals(u, v)
    }
    
    @Test
    def testToString() = {
        val v = Vector(1.0, 2.0, 3.0)
        val s = v.toString()
        assertEquals("Vector [1.0, 2.0, 3.0]", s)
    }
    
    @Test
    def testMap2() = {
        val u = Vector(2.0, 3.0, 4.0);
        val v = Vector(3.0, 4.0, 5.0);
        val w = u.map2(v)((x, y) => x + y);        
        assertEquals(Vector(5.0, 7.0, 9.0), w)
    }
    
    @Test(expected = classOf[IllegalArgumentException])
    def testMap2WrongSize() = {
        val u = Vector(2.0, 3.0, 4.0);
        val v = Vector(3.0, 4.0);
        val w = u.map2(v)((x, y) => x + y);
    }
    
    @Test
    def testMap() = {
        val u = Vector(2.0, 3.0)
        val v = u.map(x => x * x)
        assertEquals(new Vector(4.0, 9.0), v)
    }
    
    @Test
    def testReduce() = {
        val v = Vector(1.0, 2.0, 3.0)
        val r = v.reduce((x, y) => x + y)
        assertEquals(6.0, r, 0.0)
    }
    
    @Test
    def testDot() = {
        val v = Vector(1.0, 2.0, 3.0)
        val result = v.dot(v)
        assertEquals(14.0, result, 0.0)   
    }
    
    @Test
    def testMagnitude() = {
        val v = Vector(3.0, 4.0)
        val magnitude = v.magnitude        
        assertEquals(5.0, magnitude, 0.0)
    }
    
    @Test
    def testUnitize() = {
        val v = Vector(4.0, 5.0)
        val u = v.unitize()
        assertEquals(0.625, u(0), 1e-3)
        assertEquals(0.781, u(1), 1e-3)
    }
    
    @Test
    def testUnitizeZeros() = {
        val v = Vector(0.0, 0.0)
        val u = v.unitize()
        assertTrue(u(0).isNaN)
        assertTrue(u(1).isNaN)
    }
    
    @Test
    def testHashCodeDifferent() = {
        val u = Vector(1.0, 2.0)
        val v = Vector(2.0, 3.0)
        assertNotEquals(u.hashCode(), v.hashCode())
    }
    
    @Test
    def testHashCodeSame() = {
        val u = Vector(1.0, 2.0)
        val v = Vector(1.0, 2.0)
        assertEquals(u.hashCode(), v.hashCode())
    }
    
    @Test
    def testPlus() = {
        val u = Vector(1.0, 2.0)
        val v = Vector(3.0, 4.0)
        val result = u + v
        assertEquals(Vector(4.0, 6.0), result)
    }
    
    @Test(expected = classOf[IllegalArgumentException])
    def testPlusWrongSize() = {
        val u = Vector(2.0, 3.0, 4.0)
        val v = Vector(3.0, 4.0)
        val w = u + v
    }
    
    @Test
    def testMinus() = {
        val u = Vector(1.0, 2.0)
        val v = Vector(3.0, 4.0)
        val result = v - u
        assertEquals(Vector(2.0, 2.0), result)
    }
    
    @Test(expected = classOf[IllegalArgumentException])
    def testMinusWrongSize() = {
        val u = Vector(2.0, 3.0, 4.0);
        val v = Vector(3.0, 4.0);
        val w = u - v
    }
    
    @Test
    def testVectorMultiply() = {
        val u = Vector(1.0, 2.0)
        val v = Vector(3.0, 4.0)
        val result = v * u
        assertEquals(Vector(3.0, 8.0), result)
    }
    
    @Test(expected = classOf[IllegalArgumentException])
    def testVectorMultiplyWrongSize() = {
        val u = Vector(2.0, 3.0, 4.0);
        val v = Vector(3.0, 4.0);
        val w = u * v
    }
    
    @Test
    def testScalarMultiply() = {
        val v = Vector(2.0, 3.0)   
        val w = v * 3.0
        assertEquals(Vector(6.0, 9.0), w)
    }
    
    @Test
    def testScalarDivide() = {
        val v = Vector(4.0, 8.0)
        val w = v / 2.0
        assertEquals(Vector(2.0, 4.0), w)
    }
    
    @Test
    def testCrossProduct() = {
        val u = Vector(1.0, 2.0, 3.0)
        val v = u.cross(u)
        assertEquals(Vector(0.0, 0.0, 0.0), v)
    }
}