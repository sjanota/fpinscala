package chapter6

trait RNG {
  def nextInt: (Int, RNG)

  def nonNegativeInt: (Int, RNG) =
    nonNegativeIntS.toTuple

  def intDouble: ((Int, Double), RNG) =
    intDoubleS.toTuple

  private def intDoubleS =
    State(nextInt) flatMap ((i, s) => State(s.double) map ((i, _)))

  def double: (Double, RNG) =
    doubleS.toTuple

  private def doubleS: State[Double, RNG] =
    nonNegativeIntS map (_ / (Int.MaxValue.toDouble + 1))

  def nonNegativeIntS: State[Int, RNG] =
    State(nextInt).map(n => if (n >= 0) n else -(n + 1))

  def doubleInt: ((Double, Int), RNG) =
    intDoubleS.map(t => (t._2, t._1)).toTuple

  def double3: ((Double, Double, Double), RNG) =
    doubleS
      .flatMap((d1, s1) =>
        s1.doubleS.flatMap((d2, s2) => s2.doubleS.map((d1, d2, _))))
      .toTuple

  def ints(count: Int): (List[Int], RNG) = {
    @scala.annotation.tailrec
    def go(n: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (n == 0) (acc, rng)
      else {
        val (i, rng0) = rng.nextInt
        go(n - 1, rng0, i :: acc)
      }
    }

    go(count, this, Nil)
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
