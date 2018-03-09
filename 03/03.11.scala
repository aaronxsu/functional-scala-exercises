sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum(as: List[Int]): Int = foldLeft(as, 0)( _ + _ )

  def product(as: List[Double]): Double = foldLeft(as, 1.0)( _ * _ )

  def length[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)
}
