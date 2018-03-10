sealed trait Option[+A] {
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = this match {
    a.flatMap(aa => b.flatMap(bb => f(aa, bb)))
  }
}
