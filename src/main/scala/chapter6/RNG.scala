package chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  type Rand[A] = State[RNG, A]
  val Rand: State.type = State

  def apply(seed: Long): RNG = Simple(seed)

  case class Simple(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  val int: Rand[Int] = Rand(_.nextInt)

  val nonNegativeInt: Rand[Int] =
    int map (n => if (n >= 0) n else -(n + 1))

  val double: Rand[Double] =
    nonNegativeInt map (_ / (Int.MaxValue.toDouble + 1))

  val intDouble: Rand[(Int, Double)] =
    Rand.both(int, double)

  val doubleInt: Rand[(Double, Int)] =
    Rand.both(double, int)

  def double3: Rand[(Double, Double, Double)] =
    for { d1 <- double; d2 <- double; d3 <- double } yield (d1, d2, d3)

  def ints(count: Int): Rand[Seq[Int]] =
    Rand.sequence(List.fill(count)(int))

  def nonNegativeLessThen(n: Int): Rand[Int] =
    nonNegativeInt flatMap { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) Rand.unit(mod)
      else nonNegativeInt
    }

}
