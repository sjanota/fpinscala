package chapter8

import chapter6.RNG
import chapter8.Prop._

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(other: Prop): Prop = Prop { (n, rng) =>
    run(n, rng) map (_ => other.run(n, rng))
  }

  def ||(other: Prop): Prop = Prop { (n, rng) =>
    run(n, rng) orElse (() => other.run(n, rng))
  }
}

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

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

  case class Falsified(failure: FailedCase, successes: SuccessCount)
      extends Result {
    override def isFalsified: Boolean = true
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

}
