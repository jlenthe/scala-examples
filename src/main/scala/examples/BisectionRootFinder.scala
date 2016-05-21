package examples

import scala.math._

/** Interface for root finding objects */
trait RootFinder {
  
    /** Finds the roots of f bracketed by points a and b.
      *  
      * Points a and b bracket a root when signum(f(a) != signum(f(b)).  
      *  
      * @param f The function for which a root will be found
      * 
      * @param a The first bracket point
      * 
      * @param b The second bracket point
      *  
      * @return Either the approximate root of the function contained with the (a, b) bracket (Right) 
      * or an error message string (Left)
      */
    def apply(f: Double => Double, a: Double, b: Double): Either[String,Double]
}

/** Root finder that use the bisection algorithm  
  * 
  * @param tol The tolerance to which to find roots
  */
class BisectionRootFinder (tol: Double) extends RootFinder {
  
  override def apply(f: Double => Double, a: Double, b: Double): Either[String,Double] = {
    def bisect(a: Double, fa: Double, b: Double, fb: Double): Double = {
      val c = (a + b) / 2
      if (abs(a - b) < tol) {
        c
      } else {
        val fc = f(c)
        if (signum(fa) == signum(fc)) {
          val fb = f(b)
          bisect(c, fc, b, fb)   
        } else {
          bisect(a, fa, c, fc)
        }
      }
    }
    
    val fa = f(a)
    val fb = f(b)
    if (signum(fa) != signum(fb)) {
      Right(bisect(a, fa, b, fb))
    } else {
      Left(s"The input function points did not bracket the root.  First point (a, fa) = (${a}, ${fa}).  Second point (b, fb) = (${b}, ${fb}).")
    }
  }
}