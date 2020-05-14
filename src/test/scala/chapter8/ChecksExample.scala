package chapter8

import org.scalacheck.Gen
import org.scalatest.Assertions
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ChecksExample
    extends AnyPropSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with Assertions {

  property("list reversed twice is equal to itself") {
    forAll { (l: List[Int]) =>
      l.reverse.reverse should equal(l)
    }
  }
  property("head of reversed list is equal to its last element") {
    forAll { (l: List[Int]) =>
      l.reverse.headOption should equal(l.lastOption)
    }
  }

  property("sum of the reversed list is equal to sum of original list") {
    forAll { (l: List[Int]) =>
      l.reverse.sum should equal(l.sum)
    }
  }

  property("sum of list of n xs equals n * x") {
    forAll(Gen.choose(1, 100), Gen.choose(1, 10000)) { (n: Int, x: Int) =>
      List.fill(n)(x).sum should equal(n * x)
    }
  }

  property("sum of negatives equals negative sum") {
    forAll { (l: List[Int]) =>
      l.map(-1 * _).sum should equal(-l.sum)
    }
  }

  property("head + sum of tail equals sum of list") {
    forAll { (l: List[Int]) =>
      whenever(l.nonEmpty) {
        l.head + l.tail.sum should equal(l.sum)
      }
    }
  }

  property("max returns element on list") {
    forAll { (l: List[Int]) =>
      whenever(l.nonEmpty) {
        assert(l contains l.max)
      }
    }
  }

  property("max from list is last from sorted list") {
    forAll { (l: List[Int]) =>
      whenever(l.nonEmpty) {
        assert(l.max === l.sorted.last)
      }
    }
  }

  property("max is bigger or equal to every list element") {
    forAll { (l: List[Int]) =>
      whenever(l.nonEmpty) {
        assert(l forall (_ <= l.max))
      }
    }
  }
}
