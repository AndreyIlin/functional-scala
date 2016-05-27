package chapters.ch3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](h: A, t: List[A]) extends List[A]

object List {

  /**
    * Ex.2
    * Removes the first element of `l` in a constant time
    *
    * @param l given list
    * @return tail of `l`
    */
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }
  }

  /**
    * Ex.3
    * Replaces the first element of the List with a given one in a constant time
    *
    * @param h element to set instead of head element
    * @param l given list
    * @return list with head `h` and the tail of `l`
    */
  def setHead[A](h: A, l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => Cons(h, xs)
  }

  /**
    * Ex.4
    * Removes the first n elements of the given list
    *
    * @param l given list
    * @param n count of elements to delete
    * @return list without `n` first elements
    */
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  /**
    * Ex.5
    * Removes the first elements of the list until they match a predicate
    *
    * @param l given list
    * @param p predicate
    * @return `l` with elements, that matched predicate `p`, have been removed
    */
  def dropWhile[A](l: List[A])(p: A => Boolean): List[A] = {
    l match {
      case Cons(x, xs) if p(x) => dropWhile(xs)(p)
      case _ => l
    }
  }

  /**
    * Ex.6
    * Removes the last element of `l`
    *
    * @param l given list
    * @return `l` without last element
    */
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }
  }

  /**
    * Ex.9
    * Computes the length of the list `as` using foldRight function
    *
    * @param as given list
    * @return length of a `as`
    */
  def foldRightLength[A](as: List[A]): Int = {
    foldRight(as, 0)((_, acc) => acc + 1)
  }

  /**
    * Ex.10
    * Applies function `f` to all elements of `as`,
    * going left to right
    *
    * @param as given list
    * @param z  starting value
    * @param f  function to apply
    * @tparam A type of list elements
    * @tparam B function `f` result type
    * @return result of applying `f` to all elements of `as`
    */
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /**
    * Ex.11
    * Sums all elements of the list `ns` using foldLeft
    *
    * @param ns given list
    * @return sum of elements of `ns`
    */
  def foldLeftSum(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  /**
    * Ex.11
    * Product of elements of the list `ns` using foldLeft
    *
    * @param ns given list
    * @return product of elements of `ns`
    */
  def foldLeftProduct(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  /**
    * Ex.11
    * Computes the length of the list `as` using foldLeft function
    *
    * @param as given list
    * @return length of `as`
    */
  def foldLeftLength[A](as: List[A]): Int = {
    foldLeft(as, 0)((acc, _) => acc + 1)
  }

  /**
    * Ex.12
    * Reverses `as`
    *
    * @param as given list
    * @return reversed `as`
    */
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((t, h) => t match {
      case Nil => Cons(h, Nil)
      case _ => Cons(h, t)
    })
  }

  /**
    * Ex.13
    * Implements foldRight through foldLeft
    * This way fold right is tail recursive
    * Applies given function `f` to all elements of `as`,
    * going right to left
    *
    * @param as given list
    * @param z  start value
    * @param f  function to apply to each element of `as`
    * @tparam A type of list elements
    * @tparam B function `f` result type
    * @return result of applying `f` to all elements of `as`
    */
  def foldRightWithFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((b, a) => f(a, b))
  }

  /**
    * Ex.14
    * Appends one list to another
    * Implemented with foldLeft
    *
    * @param a1 first list
    * @param a2 list to append to `a1`
    * @return result of appending `a1` to `a2`
    */
  def appendFoldLeft[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(List.reverse(a1), a2)((t, h) => h match {
      case Nil => t
      case _ => Cons(h, t)
    })
  }

  /**
    * Ex.15
    * Concatenates list of lists into one single list
    *
    * @param a list of lists
    * @return result of concatenation
    */
  def concat[A](a: List[List[A]]): List[A] = a match {
    case Nil => Nil
    case Cons(h, Nil) => h
    case Cons(h, t) => List.appendFoldLeft(h, concat(t))
  }


  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldRightSum(ns: List[Int]) = foldRight(ns, 0)(_ + _)

  def foldRightProduct(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

}
