package chapters

import scala.annotation.tailrec

object Ch2 {

  object Example {
    /**
      * Tail recursion
      * Ex.1
      * Gets the nth Fibonacci number, uses local tail-recursive function
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
      * Ex.2
      * Checks whether an Array[A] is sorted according to a given comparison function
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
      * Currying
      * Ex.3 Converts a function f of two arguments into a function of one argument that partially applies f
      *
      * @param f - function to curry
      * @tparam A - type
      * @tparam B - type
      * @tparam C - type
      * @return curried function
      */
    def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
      a => b => f(a, b)
    }

    /**
      * Uncurrying
      * Ex.4
      * Converts a function f of one argument that partially applies returned function to function of two arguments
      *
      * @param f - function to uncurry
      * @tparam A - type
      * @tparam B - type
      * @tparam C - type
      * @return uncurried function
      */
    def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
      (a, b) => f(a)(b)
    }

    /**
      * Function composition
      * Ex.5
      * Function which feeds the output of one function to the input of another function
      *
      * @param f - function that composes another one
      * @param g - composed function
      * @tparam A - type
      * @tparam B - type
      * @tparam C - type
      * @return composed function
      */
    def compose[A, B, C](f: B => C, g: A => B): A => C = {
      a => f(g(a))
    }

  }

}
