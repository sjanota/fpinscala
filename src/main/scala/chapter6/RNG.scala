package chapter6
trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def apply(seed: Long): RNG = Simple(seed)

  case class Simple(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  val int: Rand[Int] = _.nextInt

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

  def ints(count: Int): Rand[List[Int]] =
    Rand.sequence(List.fill(count)(int))

  type Rand[+A] = RNG => (A, RNG)

  object Rand {
    def unit[A](a: A): Rand[A] = rng => (a, rng)

    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
      ra.map2(rb)((_, _))

    def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
      rs.foldLeft[Rand[List[A]]](Rand.unit(List())) { (acc, r) =>
        r.map2(acc)(_ :: _)
      }
  }

  implicit class RandOps[A](r: Rand[A]) {
    def map[B](f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = r(rng)
        (f(a), rng2)
      }

    def flatMap[B](f: A => Rand[B]): Rand[B] =
      rng => {
        val (a, rng2) = r(rng)
        f(a)(rng2)
      }

    def map2[B, C](rb: Rand[B])(f: (A, B) => C): Rand[C] =
      for { a <- r; b <- rb } yield f(a, b)
  }

}
