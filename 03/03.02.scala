package aaronxsu.datastructures2

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(y, ys) => ys
  }
}
