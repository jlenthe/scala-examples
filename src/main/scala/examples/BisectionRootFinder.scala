package examples

import scala.math._

class BisectionRootFinder (tol: Double) {
  def apply(f: Double => Double, a: Double, b: Double): Either[String,Double] = {
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