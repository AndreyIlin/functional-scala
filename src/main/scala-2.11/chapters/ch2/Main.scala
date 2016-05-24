package chapters.ch2

import scala.annotation.tailrec


class Main

object Main {
  /**
    * Ex.1
    * Gets the n-th Fibonacci number, uses local tail-recursive function
    *
    * @param n - Fibonacci number to count
    * @return value of `n`-th Fibonacci number
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
    * Ex.2
    * Checks whether this array is sorted according to a given comparison function
    *
    * @param as      - array to check if it's sorted
    * @param ordered - compare function
    * @tparam A - type
    * @return true if array is sorted, otherwise false
    */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @tailrec
    def loop(i: Int): Boolean = {
      if (i + 1 == as.length) true
      else if (!ordered(as(i), as(i + 1))) false
      else loop(i + 1)
    }

    if (as.length < 2) {
      true
    } else {
      loop(0)
    }
  }

  /**
    * Ex.3
    * Converts a function f of two arguments into a function of one argument that partially applies f
    *
    * @param f - function to curry
    * @return curried function
    */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  /**
    * Ex.4
    * Converts a function f of one argument that partially applies returned function to function of two arguments
    *
    * @param f - function to uncurry
    * @return uncurried function
    */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  /**
    * Ex.5
    * Feeds the output of one function to the input of another function
    *
    * @param f - function that consumes output of `g`
    * @param g - function that produces input for `f`
    * @return composed function
    */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
