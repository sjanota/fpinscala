package chapter8

import chapter6.{RNG, State}
import chapter8.Gen._

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    map2(unit(()))((x, _) => f(x))

  def map2[B, C](other: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(for {
      aa <- sample
      bb <- other.sample
    } yield f(aa, bb))
}

object Gen {
  def listOf[A](g: Gen[A]): Gen[List[A]] = ???

  def alphaNumericN(n: Int): Gen[String] =
    listOfN(n, alphaNumericChar) map (_.mkString)

  def alphaNumericChar: Gen[Char] =
    oneOf(choose(65, 90), choose(97, 122), choose(48, 57)) map (_.toChar)

  def option[A](a: Gen[A]): Gen[Option[A]] =
    boolean.map2(a)((isEmpty, aa) => if (isEmpty) None else Some(aa))

  def boolean: Gen[Boolean] =
    Gen(RNG.int map (_ % 2 == 0))

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def tuple2[A, B](a: Gen[A], b: Gen[B]): Gen[(A, B)] =
    a.map2(b)((_, _))

  def stringN(n: Int): Gen[String] =
    listOfN(n, char) map (_.mkString)

  def listOfN[A](n: Int, g: Gen[A]): Gen[Seq[A]] =
    Gen(State.traverse(List.range(0, n))(_ => g.sample))

  def char: Gen[Char] =
    choose(0, 255) map (_.toChar)

  def choose(min: Int, max: Int): Gen[Int] =
    Gen(RNG.double map (d => Math.round(d * (max - min).toDouble + min).toInt))

  def oneOf[A](as: Gen[A]*): Gen[A] = {
    val n: Gen[Int] = choose(0, as.length - 1)
    val allGen: Gen[Seq[A]] = Gen(State.sequence(as map (_.sample)))
    allGen.map2(n)(_(_))
  }
}
