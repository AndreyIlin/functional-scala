package chapters.ch3

import org.scalatest.{Matchers, WordSpec}

class ListSpec extends WordSpec with Matchers {
  "List" should {

    "remove n first elements of the list" in {
      List.drop(List(1, 2, 3, 4, 5), 2) should equal(List(3, 4, 5))
    }

    "remove first element of the list" in {
      List.tail(List(1, 2)) should equal(List(2))
    }

    "set new first element of the list" in {
      List.setHead(3, List(1, 2)) should equal(List(3, 2))
    }

    "remove first elements of list until they satisfy condition" in {
      List.dropWhile(List(3, 4, 5, 6, 7, 8))(a => a < 6) should equal(List(6, 7, 8))
    }

    "return list with all elements except last one" in {
      List.init(List(3, 4, 5, 6)) should equal(List(3, 4, 5))
    }

    "return length of the list with fold right" in {
      List.foldRightLength(List(1, 2, 3, 4, 5, 6)) should equal(6)
    }

    "return sum of list elements" in {
      List.foldLeftSum(List(1, 2, 3, 4, 5, 6, 10)) should equal(31)
    }

    "return product of list elements" in {
      List.foldLeftProduct(List(2, 2, 2, 2)) should equal(16)
    }

    "return length of the list with fold left" in {
      List.foldLeftLength(List(1, 2, 3, 4, 5, 6)) should equal(6)
    }

    "return reversed list" in {
      List.reverse(List(1, 2, 3)) should equal(List(3, 2, 1))
    }

    "return result of appending one list to another" in {
      List.appendFoldLeft(List(1, 2, 3), List(4, 5, 6)) should equal(List(1, 2, 3, 4, 5, 6))
    }

    "return concatenate list of lists into a single list" in {
      List.concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))) should equal(List(1, 2, 3, 4, 5, 6, 7, 8, 9))
    }
  }
}
