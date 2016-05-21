package chapters.second

import org.scalatest.{Matchers, WordSpec}

class ChapterSpec extends WordSpec with Matchers {

  "Chapter" should {

    "count nth Fibonacci number" in {
      Chapter.fib(13) should equal(144)
    }

    "return true if array is sorted" in {
      Chapter.isSorted(Array(10, 11, 12, 13), (a: Int, b: Int) => a < b) should be (true)
      Chapter.isSorted(Array("abcd", "bcda", "dabc", "cdab"), (a: String, b: String) => a < b) should be (false)
    }

  }
}
