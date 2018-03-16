trait RNG {
  def nextInt: (Int, RNG)
}

object Main {
  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegative(rng)
    (i.toDouble / Int.MaxValue.toDouble, rng2)
  }
}
