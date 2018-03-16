trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Main {

  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(s1: String, s2: String): String = s1 + s2
    def zero: String = ""
  }

  val listMonoid: Monoid[List[A]] = new Monoid[List[A]] {
    def op(l1: List[A], l2: List[A]): List[A] = l1 ++ l2
    def zero: List = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(i1: Int, i2: Int): Int = i1 + i2
    def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(i1: Int, i2: Int): Int = i1 * i2
    def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(b1: Boolean, b2: Boolean): Boolean = b1 || b2
    def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(b1: Boolean, b2: Boolean): Boolean = b1 && b2
    def zero: Boolean = true
  }
}
