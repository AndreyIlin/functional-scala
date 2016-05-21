package chapters.second

import org.scalatest.{Matchers, WordSpec}

class ChapterSpec extends WordSpec with Matchers {

  "Chapter" should {

    "count nth Fibonacci number" in {
      Chapter.fib(13) should equal(144)
    }

    "return true if array is sorted" in {
      Chapter.isSorted[Int](Array(10, 11, 12, 13), (a, b) => a < b) should be (true)
      Chapter.isSorted[String](Array("abcd", "bcda", "dabc", "cdab"), (a, b) => a < b) should be (false)
    }

  }
}
