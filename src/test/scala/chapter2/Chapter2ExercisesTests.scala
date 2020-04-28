package chapter2

import chapter2.Chapter2Exercises._
import org.scalatest.flatspec.AnyFlatSpec

class Chapter2ExercisesTests extends AnyFlatSpec {
  "fib" should "return 0 as first element" in {
    assert(fib(1) == 0)
  }
  "fib" should "return 1 as second element" in {
    assert(fib(2) == 1)
  }
  "fib" should "return 1 as third element" in {
    assert(fib(3) == 1)
  }
  "fib" should "return 21 as 9th element" in {
    assert(fib(9) == 21)
  }

  "isSorted" should "pass on empty array" in {
    assert(isSorted[Int](Array.empty, _ > _))
  }
  "isSorted" should "pass on one element array" in {
    assert(isSorted[Int](Array(10), _ > _))
  }
  "isSorted" should "fail on two elements in wrong order" in {
    assert(!isSorted[Int](Array(2, 5), _ > _))
  }
  "isSorted" should "pass on two elements in right order" in {
    assert(isSorted[Int](Array(2, 5), _ < _))
  }
  "isSorted" should "pass on many elements in right order" in {
    assert(isSorted[Int](Array(2, 5, 10, 234, 789), _ < _))
  }
  "isSorted" should "fail on many elements in wrong order" in {
    assert(!isSorted[Int](Array(2, 5, 10, 234, 75, 789), _ < _))
  }
}
