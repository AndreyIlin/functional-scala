package chapters.ch3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  /**
    * Ex.25
    * Calculates the size of `tree`
    * where size = leafs + branches
    *
    * @param t tree which size to calculate
    * @tparam A type of `tree` elements
    * @return size of `tree`
    */
  def size[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => 1 + size(l) + size(r)
    case Leaf(_) => 1
  }


  /**
    * Ex.26
    * Finds maximum integer value in a tree
    *
    * @param t tree to find maximum element at
    * @return maximum integer element of a tree
    */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l).max(maximum(r))
  }


  /**
    * Ex. 27
    * Calculates depth of `t`
    *
    * @param t tree which depth to calculate
    * @tparam A type of tree elements
    * @return depth of a tree
    */
  def depth[A](t: Tree[A]): Int = {
    def loop(d: Int, t: Tree[A]): Int = {
      t match {
        case Leaf(_) => d
        case Branch(l, r) => loop(d + 1, l).max(loop(d + 1, r))
      }
    }

    loop(0, t)
  }

  /**
    * This implementation of book's author is smarter but serves the same purpose
    * But it matters in case of its implementation via fold function
    */
  def depthAuthor[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depthAuthor(l) max depthAuthor(r))
  }

  /**
    * Ex.28
    * Constructs new tree applying `f` to each value in a tree
    *
    * @param t source tree to which `f` is applied
    * @param f function to apply
    * @tparam A type of tree elements of `t`
    * @tparam B type of tree elements that will be construct by applying `f` to tree values
    * @return new tree with `f` applied to each value
    */
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Branch(l, r) => Branch(Tree.map(l)(f), Tree.map(r)(f))
      case Leaf(v) => Leaf(f(v))
    }
  }

  /**
    * Ex.29
    * Applies given function to corresponding values of data constructors
    *
    * @param t source tree to fold
    * @param f function to apply to `Leaf` constructor value
    * @param g function to apply to `Branch` constructor value
    * @tparam A type of elements in `t`
    * @tparam B `fold` return type
    * @return result of applying `f` and `g` to `t`
    */
  /*
    This one was hard and I gave up. This implementation author of the book specified
   */
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  /**
    * Ex.29
    * Calculates the size of `tree`
    * where size = leafs + branches
    *
    * @param t tree which size to calculate
    * @tparam A type of `tree` elements
    * @return size of `tree`
    */
  def sizeViaFold[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)(1 + _ + _)
  }


  /**
    * Ex.29
    * Finds maximum integer value in a tree
    *
    * @param t tree to find maximum element at
    * @return maximum integer element of a tree
    */
  def maximumViaFold(t: Tree[Int]): Int = {
    fold(t)(a => a)(_ max _)
  }

  /**
    * Ex.29
    * Calculates depth of `t`
    *
    * @param t tree which depth to calculate
    * @tparam A type of tree elements
    * @return depth of a tree
    */
  def depthViaFold[A](t: Tree[A]): Int = {
    fold(t)(_ => 0)((a, b) => 1 + (a max b))
  }

}