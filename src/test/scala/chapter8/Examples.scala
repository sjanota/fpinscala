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

  "list sorted" should "work" in {
    val prop = forAll(Gen.listOf(smallInt)) { ns =>
      val sorted = ns.sorted

      @scala.annotation.tailrec
      def validate(l: List[Int]): Boolean =
        if (l.isEmpty) true
        else !l.tail.exists(_ < l.head) && validate(l.tail)

      validate(sorted)
    }

    Prop.run(prop)
  }
}
