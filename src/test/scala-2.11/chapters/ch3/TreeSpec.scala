package chapters.ch3

import org.scalatest.{Matchers, WordSpec}

class TreeSpec extends WordSpec with Matchers {

  "Tree" should {

    "return its size" in {
      val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))
      Tree.size(tree) shouldEqual 9
    }

    "return maximum integer element of the tree" in {
      val tree = Branch(Branch(Leaf(1), Leaf(20)), Branch(Leaf(351), Branch(Leaf(14), Leaf(51))))
      Tree.maximum(tree) shouldEqual 351
    }

    "return tree depth" in {
      val tree = Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
      Tree.depth(tree) shouldEqual 4
    }

    "return tree depth with authors implementation" in {
      val tree = Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
      Tree.depthAuthor(tree) shouldEqual 4
    }

    "return new tree with given function applied to each element of given tree" in {
      val tree = Branch(Branch(Leaf(1), Leaf(20)), Branch(Leaf(351), Branch(Leaf(14), Leaf(51))))
      Tree.map(tree)((_) => 1) shouldEqual Branch(Branch(Leaf(1), Leaf(1)), Branch(Leaf(1), Branch(Leaf(1), Leaf(1))))
    }

  }

}
