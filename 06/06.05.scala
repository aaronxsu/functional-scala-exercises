trait RNG {
  def nextInt: (Int, RNG)

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, r) = s(rng)
    (f(a), r)
  }
}

object Main {

  def nonNegative(rng: RNG): (Int, RNG) = {
    val (i, rng2) = RNG.nextInt
    (if i < 0  (-1) * i else i, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegative(rng)
    (i.toDouble / Int.MaxValue.toDouble, rng2)
  }


  def doubeWithMap(rng: RNG): (Double, RNG) = {
    RNG.map(nonNegative)(i => i.toDouble / Int.MaxValue.toDouble)
  }


}
