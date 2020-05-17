package chapter8

import chapter8.examples.smallInt
import org.scalatest.flatspec.AnyFlatSpec

class Examples extends AnyFlatSpec {
  "max of list" should "Work" in {
    val maxProp = forAll(Gen.nonEmptyListOf(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }

    Prop.run(maxProp)
  }
}
