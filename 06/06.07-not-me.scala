trait RNG {
  def nextInt: (Int, RNG)

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, r) = s(rng)
    (f(a), r)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r) = ra(rng)
    val (b, r2) = rb(r)
    (f(a, b), r2)
  }
}

object Main {

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, l: List[Int]): (List[Int], RNG) = {
      if count <= 0 (l, rng)
      else {
        val (i, r2) = nonNegative(r)
        go(count - 1, r2, i :: l)
      }
    }
    go(count, rng, List())
  }

  def ints2(count: Int): (List[Int], RNG) = {
    sequence(List.fill(count)(RNG.int))
  }

}
