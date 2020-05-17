package chapter8

import chapter8.SGen._

case class SGen[A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] =
    map2(unit(()))((x, _) => f(x))

  def map2[B, C](other: SGen[B])(f: (A, B) => C): SGen[C] =
    SGen(n => forSize(n).map2(other.forSize(n))(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(n => forSize(n).flatMap(f(_).forSize(n)))
}

object SGen {
  def unit[A](a: A): SGen[A] = SGen(_ => Gen.unit(a))
}
