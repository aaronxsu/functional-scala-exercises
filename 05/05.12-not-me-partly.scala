sealed trait Stream[+A] {
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((a, s)) = cons(a, unfold(s)(f))
      case None => empty
  }

  def fromWithUnfold(n: Int): Stream[int] = unfold(n)(a => Some(a, a + 1))

  def constantWithUnfold[A](a: A): Stream[A] = unfold(a)(b => Some(b, b))

  def onesWithUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))

  // not me
  def fibsWithUnfold: Stream[Int] = unfold((0, 1))(p =>
    p match {
      case (prev, curr) => Some(prev, (curr, prev + curr))
    }
  )
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
