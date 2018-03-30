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

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if as.length == 0 m.zero
    else if as.length == 1 f(as(0))
    else {
      val asx = as.splitAt(as.length / 2)
      m.op(foldMapV(asx(0), m)(f), foldMapV(asx(1), m)(f))
    }
  }

  def countString(s: String): Int = {

    def wc(c: Char): WC = {
      if c.isWhitespace Stub("")
      else Part("", 1 ,"")
    }

    def unStub(s: String) = s.length.min(1)

    foldMapV(s.toIndexedSeq, wcMonoid))(wc) match {
      case Stub(str) => unStub(str)
      case Part(l, w, r) => unStub(l) + w + unStub(r)
    }
  }
}
