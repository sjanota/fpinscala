package chapter8

import chapter6.RNG
import chapter8.Prop.Passed
import org.scalatest.flatspec.AnyFlatSpec

class PropTests extends AnyFlatSpec {
  val rng: RNG = RNG(1)
  "forAll" should "work for single predicate" in {
    val prop = forAll(Gen.choose(0, 10))(_ < 11)
    assert(prop.run(100, rng) == Passed)
  }

  "forAll" should "work for and predicates" in {
    val prop = forAll(Gen.choose(0, 10))(_ < 11) && forAll(
      Gen.listOfN(10, Gen.boolean))(_.length == 10)
    assert(prop.run(100, rng) == Passed)
  }

  "forAll" should "fail and if first predicate fails" in {
    val prop = forAll(Gen.listOfN(10, Gen.boolean))(_.length == 3) && forAll(
      Gen.choose(0, 10))(_ < 11)
    assert(prop.run(100, rng).isFalsified)
  }

  "forAll" should "fail and if second predicate fails" in {
    val prop = forAll(Gen.listOfN(10, Gen.boolean))(_.length == 10) && forAll(
      Gen.choose(0, 10))(_ < 3)
    assert(prop.run(100, rng).isFalsified)
  }

  "forAll" should "work if first predicate passes" in {
    val prop = forAll(Gen.choose(0, 10))(_ < 11) || forAll(
      Gen.listOfN(10, Gen.boolean))(_.length == 11)
    assert(prop.run(100, rng) == Passed)
  }

  "forAll" should "work if first predicate fails" in {
    val prop = forAll(Gen.choose(0, 10))(_ < 3) || forAll(
      Gen.listOfN(10, Gen.boolean))(_.length == 10)
    assert(prop.run(100, rng) == Passed)
  }

  "forAll" should "fail if both predicate fails" in {
    val prop = forAll(Gen.choose(0, 10))(_ < 3) || forAll(
      Gen.listOfN(10, Gen.boolean))(_.length == 11)
    assert(prop.run(100, rng).isFalsified)
  }
}
