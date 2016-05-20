package chapters.second

import scala.annotation.tailrec

object Chapter {

  /**
    * Ex.1 Write a recursive function to get the nth Fibonacci number
    * Implementation should use local tail-recursive function
    *
    * @param n - Fibonacci number to count
    * @return value of Fibonacci number
    */
  def fib(n: Int): Int = {

    @tailrec
    def loop(a: Int, b: Int, n: Int): Int = {
      if (n > 0) {
        loop(b, a + b, n - 1)
      } else {
        b
      }
    }

    if (n == 1) {
      0
    } else if (n == 2) {
      1
    } else {
      loop(0, 1, n - 2)
    }
  }
}
