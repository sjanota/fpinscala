package chapter3

import org.scalatest.flatspec
import org.scalatest.wordspec.AnyWordSpec

class ListTest extends AnyWordSpec {
  "hasSubsequence" should {
    "pass for empty subsequence" in {
      assert(List.hasSubsequence(List(1, 2, 3), Nil))
    }
    "pass for one element subsequence" in {
      assert(List.hasSubsequence(List(1, 2, 3), List(2)))
    }
    "pass for longer subsequence" in {
      assert(List.hasSubsequence(List(1, 2, 3, 6, 5), List(2, 3, 6)))
    }
    "pass for duplicated element" in {
      assert(List.hasSubsequence(List(1, 2, 2, 3, 6, 5), List(2, 3, 6)))
    }

    "fail for empty list" in {
      assert(!List.hasSubsequence(Nil, List(2, 3, 6)))
    }
    "fail for not matching subsequence" in {
      assert(!List.hasSubsequence(List(1, 2, 3, 6, 5), List(2, 3, 5)))
    }
  }

}
