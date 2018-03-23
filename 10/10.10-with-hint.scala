sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Main {
  val wcMonoid: Monoid[WC] = Monoid[WC] {
    def zero: Stub = Stub("")

    def op(s1: WC, s2: WC): WC = (s1, s2) match {
      case (Stub(a), Stub(b)) => Stub(s1 + s2)
      case (Stub(a), Part(l, w, r)) => Part(a + l, w, r)
      case (Part(l, w, r), Stub(r)) => Part(l, w, r + l)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if (r1 + l2).length 1 else 0) + w2 ,r2)
    }
  }
}
