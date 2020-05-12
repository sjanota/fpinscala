package chapter7

import java.util.concurrent.Executors

import org.scalatest.flatspec.AnyFlatSpec

class ExamplesTests extends AnyFlatSpec {
  "sum" should "sum numbers" in {
    val p = examples.sum(Vector(1, 2, 3, 4))
    val ec = Executors.newFixedThreadPool(4)
    assert(p(ec).get == 10)
  }
}
