package chapter8

import chapter8.Prop._

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  def &&(other: Prop): Prop = ???
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}
