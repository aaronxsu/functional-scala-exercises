package aaronxsu.datastructures4

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => sys.error("Can't drop on empty list.")
    case Cons(_, ys) => drop(ys, n - 1)
  }
}
