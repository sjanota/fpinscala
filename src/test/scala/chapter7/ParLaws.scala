package chapter7

import java.util.concurrent.Executors

import chapter8.Prop
import org.scalatest.wordspec.AnyWordSpec

class ParLaws extends AnyWordSpec {
  "map" should {
    "hold unit" in {
      val es = Executors.newCachedThreadPool
      val prop = Prop.check {
        Par.unit(1).map(_ + 1).run(es) == Par.unit(2).run(es)
      }
      Prop.run(prop)
    }
  }
}
