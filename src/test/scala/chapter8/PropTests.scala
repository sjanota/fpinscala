package chapter8

import chapter6.RNG
import chapter8.Prop._
import org.scalatest.flatspec.AnyFlatSpec

class PropTests extends AnyFlatSpec {
  val rng: RNG = RNG(1)
  "forAll" should "work for single predicate" in {
    val prop = forAll(Gen.choose(0, 10))(_ < 11)
    assert(prop.run(100, 1, rng) == Passed)
  }

  "forAll" should "work for and predicates" in {
    val prop = forAll(Gen.choose(0, 10))(_ < 11) && forAll(
      Gen.listOfN(10, Gen.boolean))(_.length == 10)
    assert(prop.run(100, 1, rng) == Passed)
  }

  "forAll" should "fail and if first predicate fails" in {
    val prop = forAll(Gen.listOfN(10, Gen.boolean))(_.length == 3) && forAll(
      Gen.choose(0, 10))(_ < 11)
    assert(prop.run(100, 1, rng).isFalsified)
  }

  "forAll" should "fail and if second predicate fails" in {
    val prop = forAll(Gen.listOfN(10, Gen.boolean))(_.length == 10) && forAll(
      Gen.choose(0, 10))(_ < 3)
    assert(prop.run(100, 1, rng).isFalsified)
  }

  "forAll" should "work if first predicate passes" in {
    val prop = forAll(Gen.choose(0, 10))(_ < 11) || forAll(
      Gen.listOfN(10, Gen.boolean))(_.length == 11)
    assert(prop.run(100, 1, rng) == Passed)
  }

  "forAll" should "work if first predicate fails" in {
    val prop = forAll(Gen.choose(0, 10))(_ < 3) || forAll(
      Gen.listOfN(10, Gen.boolean))(_.length == 10)
    assert(prop.run(100, 1, rng) == Passed)
  }

  "forAll" should "fail if both predicate fails" in {
    val prop = forAll(Gen.choose(0, 10))(_ < 3) || forAll(
      Gen.listOfN(10, Gen.boolean))(_.length == 11)
    assert(prop.run(100, 1, rng).isFalsified)
  }

  "forAll" should "include property name in failure" in {
    val name = "is smaller then 3"
    val prop = forAll(Gen.choose(0, 10), name)(_ < 3)
    assert(prop.run(100, 1, rng).asInstanceOf[Falsified].propName == name)
  }

  "&&" should "preserve prop name on failure" in {
    val prop = forAll(Gen.listOfN(10, Gen.boolean), "prop1")(_.length == 3) && forAll(
      Gen.choose(0, 10),
      "prop2")(_ < 11)
    assert(prop.run(100, 1, rng).asInstanceOf[Falsified].propName == "prop1")
  }

  "||" should "preserve prop name on failure" in {
    val prop = forAll(Gen.listOfN(10, Gen.boolean), "prop1")(_.length == 10) && forAll(
      Gen.choose(0, 10),
      "prop2")(_ < 3)
    assert(prop.run(100, 1, rng).asInstanceOf[Falsified].propName == "prop2")
  }
}
