package chapters.second

import scala.annotation.tailrec

object Chapter {

  /**
    * Tail recursion
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

  /**
    * Polymorphic functions
    * Ex.2 Implement isSorted,
    * which checks whether an Array[A] is sorted according to a given comparison function
    */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @tailrec
    def loop(i: Int): Boolean = {
      if (i + 1 < as.length) {
        if (ordered(as(i), as(i + 1))) {
          loop(i + 1)
        } else {
          false
        }
      } else {
        true
      }
    }

    if (as.length < 2) {
      true
    } else {
      loop(0)
    }
  }
}
