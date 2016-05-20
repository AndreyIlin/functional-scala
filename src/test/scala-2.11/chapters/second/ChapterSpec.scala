package chapters.second

import org.scalatest.{Matchers, WordSpec}

class ChapterSpec extends WordSpec with Matchers {

  "ChapterSpec" should {

    "Count nth Fibonacci number" in {
      Chapter.fib(13) should equal(144)
    }

  }
}
