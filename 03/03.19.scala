sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => {
        if (f(h)) buf += h
        go(t)
      }
    }
    go(l)
    List(buf.toList: _*)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def filterUseFold[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)
  }
}
