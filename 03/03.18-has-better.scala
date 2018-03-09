sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])(Cons(f(_), _))

  // below solution is from:
  // https://github.com/fpinscala/fpinscala/blob/master/answerkey/datastructures/18.answer.scala
  // since the foldRight function above is not stack safe
  // below is implemented using local mutation that isn't observable outside
  //the function, since we're only mutating a buffer that we've allocated.
  def map_2[A,B](l: List[A])(f: A => B): List[B] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[B]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => buf += f(h); go(t)
    }
    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }
}
