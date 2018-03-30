trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Main {

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if as.length == 0 m.zero
    else if as.length == 1 f(as(0))
    else {
      val asx = as.splitAt(as.length / 2)
      m.op(foldMapV(asx(0), m)(f), foldMapV(asx(1), m)(f))
    }
  }
}
