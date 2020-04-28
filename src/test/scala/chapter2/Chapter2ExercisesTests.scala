package chapter2

import org.scalatest.flatspec.AnyFlatSpec

class Chapter2ExercisesTests extends AnyFlatSpec {
  "fib" should "return 0 as first element" in {
    assert(Chapter2Exercises.fib(1) == 0)
  }
  "fib" should "return 1 as second element" in {
    assert(Chapter2Exercises.fib(2) == 1)
  }
  "fib" should "return 1 as third element" in {
    assert(Chapter2Exercises.fib(3) == 1)
  }
  "fib" should "return 21 as 9th element" in {
    assert(Chapter2Exercises.fib(9) == 21)
  }
}
