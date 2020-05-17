package chapter8

import chapter8.Prop._

object examples {
  val intList = Gen.listOf(Gen.choose(0, 100))
  val prop =
    forAll(intList)(ns => ns.reverse.reverse == ns) &&
      forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)

}
