sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def maximum[A](t: Tree[A]): A = t match {
  case Leaf(l) => l
  case Branch(t1, t2) => maximum(t1) max maximum(t2)
}
