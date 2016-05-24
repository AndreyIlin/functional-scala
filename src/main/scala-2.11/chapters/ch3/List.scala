package chapters.ch3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](h: A, t: List[A]) extends List[A]

object List {

  /**
    *
    * @param l
    * @tparam A
    * @return
    */
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }
  }

  /**
    *
    * @param l
    * @param n
    * @tparam A
    * @return
    */
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  /**
    *
    * @param l
    * @param p
    * @tparam A
    * @return
    */
  def dropWhile[A](l: List[A])(p: A => Boolean): List[A] = {
    l match {
      case Cons(x, xs) if p(x) => dropWhile(xs)(p)
      case _ => l
    }
  }

  /**
    *
    * @param h
    * @param l
    * @tparam A
    * @return
    */
  def setHead[A](h: A, l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => Cons(h, xs)
  }

  /**
    *
    * @param l
    * @tparam A
    * @return
    */
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }
  }

  /**
    *
    * @param as
    * @tparam A
    * @return
    */
  def foldRightLength[A](as: List[A]): Int = {
    foldRight(as, 0)((_, acc) => acc + 1)
  }

  /**
    *
    * @param as
    * @param z
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /**
    *
    * @param ns
    * @return
    */
  def foldLeftSum(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  /**
    *
    * @param ns
    * @return
    */
  def foldLeftProduct(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  /**
    *
    * @param as
    * @tparam A
    * @return
    */
  def foldLeftLength[A](as: List[A]): Int = {
    foldLeft(as, 0)((acc, _) => acc + 1)
  }

  /**
    *
    * @param as
    * @param z
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def foldRightWithFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((b, a) => f(a, b))
  }

  /**
    *
    * @param as
    * @tparam A
    * @return
    */
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((t, h) => t match {
      case Nil => Cons(h, Nil)
      case _ => Cons(h, t)
    })
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
