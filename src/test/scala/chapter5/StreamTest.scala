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
}
