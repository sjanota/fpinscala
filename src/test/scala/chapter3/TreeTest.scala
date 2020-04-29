package chapter3

import chapter3.Tree._
import org.scalatest.wordspec.AnyWordSpec

class TreeTest extends AnyWordSpec {
  "size" should {
    "return 1 for a leaf" in {
      assert(Leaf(1).size == 1)
    }
    "return 3 for branch with to leaves" in {
      assert(Branch(Leaf(1), Leaf(2)).size == 3)
    }

    "return number of nodes for a complex tree" in {
      assert(
        Branch(
          Leaf(1),
          Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(2)))).size == 9)
    }
  }

  "maximum" should {
    "return leaf value" in {
      assert(Leaf(1).maximum == 1)
    }
    "return bigger of two leaves" in {
      assert(Branch(Leaf(1), Leaf(2)).maximum == 2)
      assert(Branch(Leaf(5), Leaf(3)).maximum == 5)
    }
    "return biggest number in a complex tree" in {
      assert(Branch(Leaf(1),
                    Branch(Branch(Leaf(6), Leaf(57)),
                           Branch(Leaf(3), Leaf(17)))).maximum == 57)
    }
  }

  "depth" should {
    "return 1 for leaf" in {
      assert(Leaf(1).depth == 0)
    }
    "return deepest depth in a complex tree" in {
      assert(
        Branch(Leaf(1),
               Branch(Branch(Leaf(6), Leaf(57)),
                      Branch(Leaf(3), Branch(Leaf(3), Leaf(17))))).depth == 4)
    }
  }

  "map" should {
    "change all values" in {
      assert(
        Branch(Leaf(1), Branch(Branch(Leaf(6), Leaf(57)), Leaf(3)))
          .map(_.toString) == Branch(Leaf("1"),
                                     Branch(Branch(Leaf("6"), Leaf("57")),
                                            Leaf("3"))))
    }
  }
}
