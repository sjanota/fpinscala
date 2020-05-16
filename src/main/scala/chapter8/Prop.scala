package chapter8

import chapter6.RNG
import chapter8.Prop._

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(other: Prop): Prop = ???
}

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount)
      extends Result {
    override def isFalsified: Boolean = true
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

}
