package chapters.ch2

import org.scalatest.{Matchers, WordSpec}

class MainSpec extends WordSpec with Matchers {
  "Main" should {

    "count nth Fibonacci number" in {
      Main.fib(13) should equal(144)
    }

    "return true if array is sorted" in {
      Main.isSorted[Int](Array(10, 11, 12, 13), (a, b) => a < b) should be(true)
      Main.isSorted[String](Array("abcd", "bcda", "dabc", "cdab"), (a, b) => a < b) should be(false)
    }

  }
}
