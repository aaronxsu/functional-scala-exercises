sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  // The goal of this exercise is to write this reverse method using fold
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc, h) => Cons(h, acc)))
  /*
    so the trace is like this:
    foldLeft(List(2, 3), Cons(1, List())(f)
    foldLeft(List(3), Cons(2, List(1)))(f)
    foldLeft(List(), Cons(3, List(2, 1)))(f)
    Cons(3, 2, 1)
  */
}
