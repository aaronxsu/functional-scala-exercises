package aaronxsu.datastructures4

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => sys.error("Can't drop on empty list.")
    case Cons(h, t) => {
      if (f(h)) dropWhile(t, f)
      else l
  }
}
