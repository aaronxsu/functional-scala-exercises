sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def depth[A](t: Tree[A]): Int = t match {
  case Leaf(_) => 0
  case Branch(t1, t2) => depth(t1).max(depth(t2)) + 1
}
