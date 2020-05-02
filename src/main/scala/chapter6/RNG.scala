package chapter6

import chapter6.RNG._
trait RNG {
  def nextInt: (Int, RNG)

  val int: Rand[Int] = _.nextInt

  val nonNegativeInt: Rand[Int] =
    int map (n => if (n >= 0) n else -(n + 1))

  val double: Rand[Double] =
    nonNegativeInt map (_ / (Int.MaxValue.toDouble + 1))

  val intDouble: Rand[(Int, Double)] =
    for { i <- int; d <- double } yield (i, d)

  val doubleInt: Rand[(Double, Int)] =
    intDouble map (t => (t._2, t._1))

  def double3: Rand[(Double, Double, Double)] =
    for { d1 <- double; d2 <- double; d3 <- double } yield (d1, d2, d3)

  def ints(count: Int): Rand[List[Int]] = {
    @scala.annotation.tailrec
    def go(n: Int, acc: Rand[List[Int]]): Rand[List[Int]] = {
      if (n == 0) Rand.unit(List())
      else go(n - 1, acc flatMap (is => int map (_ :: is)))
    }

    go(count, Rand.unit(List()))
  }
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

  type Rand[+A] = RNG => (A, RNG)

  object Rand {
    def unit[A](a: A): Rand[A] = rng => (a, rng)

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
  }

}

case class State[A, S](a: A, s: S) {

  def map[B](f: A => B): State[B, S] =
    State(f(a), s)

  def mapState[S1](f: S => S1): State[A, S1] =
    State(a, f(s))

  def flatMap[B](f: (A, S) => State[B, S]): State[B, S] =
    f(a, s)

  def toTuple: (A, S) =
    (a, s)
}

object State {
  def apply[A, S](t: (A, S)): State[A, S] = State(t._1, t._2)
}
