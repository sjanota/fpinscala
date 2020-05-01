package chapter5

import org.scalatest.wordspec.AnyWordSpec

class StreamTest extends AnyWordSpec {
  "takeWhile" should {
    "return matching elements" in {
      assert(Stream(1, 2, 3).takeWhile(_ < 3).toList == List(1, 2))
    }
    "return empty stream if first doesn't match" in {
      assert(Stream(3, 1, 2, 3).takeWhile(_ < 3).toList == List())
    }
  }

  "forAll" should {
    "check finite stream" in {
      assert(Stream(1, 2, 3) forAll (_ < 5))
      assert(!(Stream(1, 2, 3) forAll (_ < 2)))
    }

    "check infinite stream" in {
      assert(!(infiniteStream forAll (_ < 10)))
    }
  }

  "takeWhile" should {
    "check finite stream" in {
      assert(Stream(1, 2, 3).takeWhile(_ < 5).toList == List(1, 2, 3))
      assert(Stream(1, 2, 3).takeWhile(_ < 2).toList == List(1))
    }

    "check infinite stream" in {
      assert(infiniteStream.takeWhile(_ < 5).toList == List(0, 1, 2, 3, 4))
    }
  }

  def infiniteStream: Stream[Int] = {
    def next(i: Int): Stream[Int] =
      Stream.cons(i, next(i + 1))

    next(0)
  }
}
