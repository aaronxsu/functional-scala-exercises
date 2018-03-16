trait RNG {
  def nextInt: (Int, RNG)
}

object Main {
  def nonNegative(rng: RNG): (Int, RNG) = {
    val (i, rng2) = RNG.nextInt
    (if i < 0  (-1) * i else i, rng2)
  }
}
