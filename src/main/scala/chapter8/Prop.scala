package chapter8

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
    }
  }

}
