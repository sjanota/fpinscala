package chapter2

import org.scalatest.funspec.AnyFunSpec

class Chapter2ExercisesTests extends AnyFunSpec {
  describe("fib") {
    it("should return 0 as first element") {
      assert(Chapter2Exercises.fib(1) == 0)
    }
    it("should return 1 as second element") {
      assert(Chapter2Exercises.fib(2) == 1)
    }
    it("should return 1 as third element") {
      assert(Chapter2Exercises.fib(3) == 1)
    }
    it("should return 21 as 8 element") {
      assert(Chapter2Exercises.fib(8) == 21)
    }
  }
}
