sealed trait Option[+A] {
  // apply f if the option is not None
  def map[B](f: A  => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
  // apply f, which may fail, to the Option if not None
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
