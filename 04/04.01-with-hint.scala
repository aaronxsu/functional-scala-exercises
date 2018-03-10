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
  // B >: A says that B type parameter must be equal to or a supertype of A
  // default: => B means that the arguments is of type B,
  // and it wont be evaluated until needed by the function
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }
  // dont evaluate ob unless needed
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }
  // convert Some to None if the value does not satisfy f
  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) if (f(a)) => this
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
