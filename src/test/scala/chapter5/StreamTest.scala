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

    "check infinite stream" in {
      assert(Stream.from(0).takeWhile(_ < 5).toList == List(0, 1, 2, 3, 4))
    }
  }

  "forAll" should {
    "check finite stream" in {
      assert(Stream(1, 2, 3) forAll (_ < 5))
      assert(!(Stream(1, 2, 3) forAll (_ < 2)))
    }

    "check infinite stream" in {
      assert(!(Stream.from(0) forAll (_ < 10)))
    }
  }

  "headOption" should {
    "return head if non-empty" in {
      assert(Stream.from(0).headOption.contains(0))
    }

    "return None if empty" in {
      assert(Stream.empty.headOption.isEmpty)
    }

  }

  "map" should {
    "check finite stream" in {
      assert(Stream(1, 2, 3).map(_ * 2).toList == List(2, 4, 6))
    }

    "check infinite stream" in {
      assert(Stream.from(0).map(_ * 2).take(3).toList == List(0, 2, 4))
    }
  }

  "append" should {
    "join two streams" in {
      assert(
        Stream(1, 2, 3).append(Stream(4, 5, 6)).toList == List(1, 2, 3, 4, 5,
          6))
    }
  }

  "flatMap" should {
    "check finite stream" in {
      assert(
        Stream(1, 2, 3).flatMap(x => Stream(x, x)).toList == List(1, 1, 2, 2, 3,
          3))
    }

    "check infinite stream" in {
      assert(
        Stream.from(0).flatMap(x => Stream(x, x)).take(3).toList == List(0,
                                                                         0,
                                                                         1))
    }
  }

  "filter" should {
    "skip elements in stream" in {
      assert(Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList == List(2, 4))
    }
  }

  "fibs" should {
    "return Fibonacci numbers" in {
      assert(Stream.fibs.take(8).toList == List(0, 1, 1, 2, 3, 5, 8, 13))
    }
  }

  "zipWith" should {
    "zip two streams" in {
      val s1 = Stream(1, 2, 3, 4)
      val s2 = Stream("a", "b", "c")
      assert(
        s1.zipWith(s2)((_, _)).toList == List((1, "a"), (2, "b"), (3, "c")))
    }
  }

  "zipAll" should {
    "zip two streams" in {
      val s1 = Stream(1, 2, 3, 4)
      val s2 = Stream("a", "b", "c")
      assert(
        s1.zipAll(s2).toList == List((Some(1), Some("a")),
                                     (Some(2), Some("b")),
                                     (Some(3), Some("c")),
                                     (Some(4), None)))
    }
  }
}
