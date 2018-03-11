sealed trait Stream[+A] {
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t(h).foldRight(z)(f))
    case _ => z
  }

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t)

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) =>
    if f(h) cons(h, t)
    else t
  )

  // not me
  def append[B>:A](s: => Stream[B]): Stream[B] = foldRight(s)((h,t) => cons(h,t))

  // not me
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h).append(t))
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
