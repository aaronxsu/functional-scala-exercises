trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Main {

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(e1: A => A, e2: A => A): A => A = e1.compose(e2)
    def zero: A => A = (a: A) => a
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  def firstFunctionMonoid[A]: Monoid[A => A] = endoMonoid[A]
  def lastFunctionMonoid[A]: Monoid[A => A] = dual(firstFunctionMonoid)

}
