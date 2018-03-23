trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Main {
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
  }
}
