package chapter4

import org.scalatest.wordspec.AnyWordSpec

class OptionTest extends AnyWordSpec {
  "map" should {
    "map some value" in {
      assert(Some(1).map(_ * 2) == Some(2))
    }

    "pass through none" in {
      assert((None: Option[Int]).map(_ * 2) == None)
    }
  }

  "getOrElse" should {
    "return some value" in {
      assert(Some(1).getOrElse(2) == 1)
    }

    "return default if none" in {
      assert(None.getOrElse(2) == 2)
    }
  }

  "flatMap" should {
    "flatten some value" in {
      assert(Some(2).flatMap(x => Some(x * 2)) == Some(4))
    }

    "flatten none" in {
      assert(Some(2).flatMap(_ => None) == None)
    }

    "ignore none" in {
      assert(None.flatMap(_ => Some(2)) == None)
    }
  }

  "orElse" should {
    "pass through on some" in {
      assert(Some(2).orElse(None) == Some(2))
    }

    "use default on none" in {
      assert(None.orElse(Some(3)) == Some(3))
    }
  }

  "filter" should {
    "pass through if predicate matches" in {
      assert(Some(2).filter(_ % 2 == 0) == Some(2))
    }

    "return none if predicate doesn't match" in {
      assert(Some(2).filter(_ % 2 == 1) == None)
    }

    "ignore none" in {
      assert((None: Option[Int]).filter(_ % 2 == 0) == None)
    }
  }

}
