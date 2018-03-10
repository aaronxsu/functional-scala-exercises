sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }
}

sealed trait Option[+A] {
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = this match {
    a.flatMap(aa => b.flatMap(bb => f(aa, bb)))
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  /*
  Here's an explicit recursive version:
  */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t).map(_ => hh :: _))
    }
  /*
  It can also be implemented using `foldRight` and `map2`. The type annotation on `foldRight` is needed here; otherwise Scala wrongly infers the result type of the fold as `Some[Nil.type]` and reports a type error (try it!). This is an unfortunate consequence of Scala using subtyping to encode algebraic data types.
  */
  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((h,t) => map2(h, t)(_ :: _))
}
