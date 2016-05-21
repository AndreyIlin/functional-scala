package chapters

import org.scalatest.{Matchers, WordSpec}

class Ch2$Spec extends WordSpec with Matchers {

  "Example" should {

    "count nth Fibonacci number" in {
      Ch2.Example.fib(13) should equal(144)
    }

    "return true if array is sorted" in {
      Ch2.Example.isSorted[Int](Array(10, 11, 12, 13), (a, b) => a < b) should be (true)
      Ch2.Example.isSorted[String](Array("abcd", "bcda", "dabc", "cdab"), (a, b) => a < b) should be (false)
    }

  }
}
