sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def append[A](l: List[A], a: A): List[A] = foldRight(l, a)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)
}
