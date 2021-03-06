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

/** An immutable double-precision math vector of any dimension 
  *  
  * @constructor Creates the vector using the provided Scala library Vector
  */
class Vector(val data: scala.collection.immutable.Vector[Double]) {
    
    /** Creates the vector using the provided Doubles as the vector elements. 
      * @param d Variable number of double arguments to place in the vector
      */
    def this(d: Double*) = this(d.toVector)

    /** Invokes the provided function on corresponding pairs of elements from this Vector
      * and the Vector argument (that).
      * 
      * @param that the other vector to map elements to.  It must contain the same number of elements are this
      * Vector or a Left[Exception] is returned.
      * 
      * @return Either the Vector that results when f is invoked on corresponding pairs of Doubles from this
      * and that or an error message describing the error that occurred.
      */
    def map2(that: Vector)(f: (Double, Double) => Double): Vector =
        if (this.data.length == that.data.length)
            new Vector(((this.data, that.data)).zipped.map(f))
        else
            throw new IllegalArgumentException(s"Vector operation performed mismatching vector length. First length: ${data.length}. Second length: ${that.data.length}")

    /** Invokes the provided function on every element of the Vector producing a new Vector 
      *
      * @param f The function that is invoked on each element of this Vector
      * 
      * @return A new Vector containing the results of all the function invocations  
      */
    def map(f: Double => Double): Vector = new Vector(data.map(f))

    /** Reduces the vector using a reduction function.
      * 
      * @param f The reduction function
      * 
      * @return The accumulated result
      */
    def reduce(f: (Double, Double) => Double): Double = data.reduce(f)

    /** Gets the ith vector element
      * 
      * @param i The index of the Vector element to get
      */
    def apply(i: Int): Double = data.apply(i)

    /** Computes the [[https://en.wikipedia.org/wiki/Dot_product dot]] product of this Vector 
      *  and the provided Vector
      *
      * @param v The other vector in the dot product operation.  It must contain the same 
      * number of elements are this Vector or a Left[Exception] is returned.
      * 
      * @return Either the dot product of this and v or an Exception describing
      * the error that occurred.
      */
    def dot(v: Vector): Double = map2(v)(_ * _).reduce(_ + _)

    /** Computes the magnitude (that is [[https://en.wikipedia.org/wiki/Norm_(mathematics)#Euclidean_norm Euclidean norm]])
      * of this Vector. 
      */
    def magnitude(): Double = Math.sqrt(map(Math.pow(_, 2.0)).reduce(_ + _))

    /** Computes a unit vector by dividing this Vector by its magnitude. */
    def unitize(): Vector = this / this.magnitude

    /** Adds this Vector to another.
      *
      * @param v The other vector to add.  It must contain the same 
      * number of elements are this Vector or a Left[Exception] is returned.   
      */
    def +(v: Vector): Vector = map2(v)(_ + _)

    /** Subtracts another Vector from this Vector.
      *  
      * @param v The vector to subtract from this Vector.  It must contain the same 
      * number of elements are this Vector or a Left[Exception] is returned.  
      */
    def -(v: Vector): Vector = map2(v)(_ - _)

    /** Multiples this Vector by another vector component-wise. */
    def *(v: Vector): Vector = map2(v)(_ * _)

    /** Multiplies this vector by a scalar. */
    def *(s: Double): Vector = map(_ * s)

    /** Divides this vector by a scalar. */
    def /(s: Double): Vector = map(_ / s)

    /** The number of dimensions of this vector, that is, how many elements it has */
    def length = data.length

    /** Computes the [[https://en.wikipedia.org/wiki/cross_product cross]] product of this Vector 
      * and the provided Vector.  This method must be called for a 3 element vector only or else
      * an exception is returned in a Left.
      *
      * @param v The other vector in the cross product operation.  It must contain exactly 3 elements.
      * 
      * @return Either the dot product of this and v or an Exception describing
      * the error that occurred.
      */
    def cross(that: Vector): Vector =
        if (Vector.this.length == 3 && that.length == 3)
            new Vector(this(1) * that(2) - this(2) * that(1), 
                       this(2) * that(0) - this(0) * that(2),
                       this(0) * that(1) - this(1) * that(0))
        else
            throw new Exception("Both Vecs must be 3 element for cross product.")

    /** Determines if this Vector is equivalent to the provided object. */
    override def equals(a: Any) = 
        a match {
            case v: Vector => this.data == v.data
            case _ => false
        }
    
    /** Computes a hash code for this object. */
    override def hashCode = data.hashCode
            
    /** Converts this Vector to a string representation. */
    override def toString =
        data.tail.fold("Vector [" + data.head.toString)((s, d) => s + ", " + d.toString) + "]"
}

object Vector {
    
    /** Creates the vector using the provided Doubles as the vector elements. 
      * @param d Variable number of double arguments to place in the vector
      */
    def apply(d: Double*) = new Vector(d.toVector)
}