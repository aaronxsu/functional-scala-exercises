trait RNG {
  def nextInt: (Int, RNG)
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

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = nonNegative(rng)
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = nonNegative(rng2)
    ((d, i), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }


}
