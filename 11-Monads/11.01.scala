trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b))
}

object Main {
  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] =
      Some(a)

    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma.flatMap(f)
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)

    def flatMap[A, B](ma: List[A])(f: A => List[B]): Option[B] =
      ma.flatMap(f)
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)

    def flatMap[A, B](ma: Stream)(f: A => Stream[B]): Stream[B] =
      ma.flatMap(f)
  }
}
