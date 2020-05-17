package chapter8

import chapter5.Stream
import chapter6.RNG
import chapter8.Prop._

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(other: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) map (_ => other.run(max, n, rng))
  }

  def ||(other: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) orElse (() => other.run(max, n, rng))
  }
}

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean

    def orElse(f: () => Result): Result = {
      if (isFalsified) f()
      else this
    }

    def map(f: Result => Result): Result = {
      if (!isFalsified) f(this)
      else this
    }
  }

  case class Falsified(propName: String,
                       failure: FailedCase,
                       successes: SuccessCount)
      extends Result {
    override def isFalsified: Boolean = true
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

  def run(p: Prop,
          maxSize: MaxSize = 100,
          testCases: TestCases = 100,
          rng: RNG = RNG(System.currentTimeMillis())): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(propName, msg, successes) =>
        println(s"! Falsified after $successes passed tests:\n $msg")
      case Passed =>
        println(s"+ OK. Passed $testCases test cases.")
      case Proved =>
        println(s"+ OK. Proved property.")
    }
  }

  def forAll[A](g: SGen[A])(p: A => Boolean): Prop = forAll(g, "")(p)

  def forAll[A](g: SGen[A], name: String)(p: A => Boolean): Prop =
    Prop { (maxSize, n, rng) =>
      streamOfSizes(maxSize, n)
        .map(i => forAll(g.forSize(i), name)(p))
        .map(prop =>
          Prop { (mazSize, _, rng) =>
            prop.run(mazSize, 1, rng)
        })
        .toList
        .reduce(_ && _)
        .run(maxSize, n, rng)
    }

  def streamOfSizes(maxSize: MaxSize, n: TestCases): Stream[Int] = {
    val step = maxSize / n.toDouble
    Stream
      .unfold(0) { i =>
        if (i > n) None
        else Some((Math.round(i * step).toInt, i + 1))
      }
      .take(n)
  }

  def forAll[A](g: Gen[A])(p: A => Boolean): Prop = forAll(g, "")(p)

  def forAll[A](g: Gen[A], name: String)(p: A => Boolean): Prop = Prop {
    (n, _, rng) =>
      randomStream(g, rng) zip Stream.from(0) take n map {
        case (a, i) =>
          try {
            if (p(a)) Passed
            else Falsified(name, a.toString, i)
          } catch {
            case e: Exception => Falsified(name, buildMsg(a, e), i)
          }
      } find (_.isFalsified) getOrElse Passed
  }

  def buildMsg[A](a: A, e: Exception): String =
    s"test case: $a\n" +
      s"generated an exception: $e\n" +
      s"stack trace:\n${e.getStackTrace.mkString("\n")}"

  def randomStream[A](g: Gen[A], rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample(rng)))

  def check(p: => Boolean, name: String = ""): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified(name, "()", 0)
  }

  case object Proved extends Result {
    override def isFalsified: Boolean = false
  }

}
