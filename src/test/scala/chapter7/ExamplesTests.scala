package chapter7

import java.util.concurrent.Executors

import org.scalatest.flatspec.AnyFlatSpec

class ExamplesTests extends AnyFlatSpec {
  private val ec = Executors.newSingleThreadExecutor()

  "sum" should "sum numbers" in {
    val p = examples.sum(Vector(1, 2, 3, 4))
    assert(p.run(ec) == 10)
  }

  "max" should "pick highest number" in {
    val p = examples.max(Vector(1, 5, 2, 7))
    assert(p.run(ec).contains(7))
  }

  "max" should "pick the only number" in {
    val p = examples.max(Vector(3))
    assert(p.run(ec).contains(3))
  }

  "max" should "return none on empty seq" in {
    val p = examples.max(Vector())
    assert(p.run(ec).isEmpty)
  }

  "sortPar" should "sort numbers" in {
    val p = examples.sortPar(Par.unit(List(3, 1, 4, 2)))
    assert(p.run(ec) == List(1, 2, 3, 4))
  }

  "parFilter" should "filter even numbers" in {
    val p = examples.parFilter(List(3, 1, 4, 2))(_ % 2 == 0)
    assert(p.run(ec) == List(2, 4))
  }

  "countWords" should "count words" in {
    val p = examples.countWords(
      List(
        "Can you think of any other useful functions to write?",
        "Experiment with writing a few.",
        "Here are some ideas:"
      ))
    assert(p.run(ec) == 19)
  }

  "map2" should "not block" in {
    assert(Par.lazyUnit(3).map2(Par.lazyUnit(2))(_ + _).run(ec) == 5)
  }

  "choice" should "chose" in {
    assert(Par.choice(Par.unit(true))(Par.unit(1), Par.unit(2)).run(ec) == 1)
    assert(Par.choice(Par.unit(false))(Par.unit(1), Par.unit(2)).run(ec) == 2)
  }
}
