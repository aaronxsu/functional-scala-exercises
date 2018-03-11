sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Cons(h, t) => h :: toList(t)
    case _ => List()
  }

  // Has better:
  /*
  The above solution will stack overflow for large streams, since it's
  not tail-recursive. Here is a tail-recursive implementation. At each
  step we cons onto the front of the `acc` list, which will result in the
  reverse of the stream. Then at the end we reverse the result to get the
  correct order again.
  [:ben] are the line breaks above okay? I'm unclear on whether these "hints" are supposed to go in the book or not
  */
  def toList2: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]) = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse()
  }

  /*
  In order to avoid the `reverse` at the end, we could write it using a
  mutable list buffer and an explicit loop instead. Note that the mutable
  list buffer never escapes our `toList` method, so this function is
  still _pure_.
  */

  def toList2: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    def go(s: Stream[A]) = s match {
      case Cons(h, t) => buf += h(); go(t())
      case _ => buf.toList
    }
    go(this)
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
