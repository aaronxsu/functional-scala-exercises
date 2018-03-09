sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// def size[A](t: Tree[A]): Int = t match {
//   case Leaf(_) => 1
//   case Branch(t1, t2) => size(t1) + size(t2) + 1
// }
//
// def maximum[A](t: Tree[A]): A = t match {
//   case Leaf(l) => l
//   case Branch(t1, t2) => maximum(t1) max maximum(t2)
// }
//
// def depth[A](t: Tree[A]): Int = t match {
//   case Leaf(a) => 0
//   case Branch(t1, t2) => depth(t1).max(depth(t2)) + 1
// }
//
// def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
//   case Leaf(a) => Leaf(f(a))
//   case Branch(l,r) => Branch(map(l)(f), map(r)(f))
// }

// with hint:
def fold[A, B](t: Tree[A])(l: A => B)(b: (B, B) => B): B = t match {
  case Leaf(a) => l(a)
  case Branch(t1, t1) => b(fold(t1)(l)(b),fold(t2)(l)(b))
}

def size[A](t: Tree[A]): Int = fold(t)(a => 1)( _ + _ + 1)

def maximum[A](t: Tree[A]): A = fold(t)(a => a)(_ max _)

def depth[A](t: Tree[A]): Int = fold(t)(a => 0)(_.max(_) + 1)

// with hint:
def map[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
