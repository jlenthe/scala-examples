package examples

import scala.math._

class BisectionRootFinder (tol: Double) {
  def apply(f: Double => Double, a: Double, b: Double): Double = {
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
    bisect(a, fa, b, fb)
  }
}