import chapter5.Stream
import chapter6.RNG
import chapter8.Prop.{Falsified, MaxSize, Passed, TestCases}

package object chapter8 {
  def forAll[A](g: SGen[A])(p: A => Boolean): Prop = forAll(g, "")(p)

  def forAll[A](g: SGen[A], name: String)(p: A => Boolean): Prop =
    Prop { (maxSize, n, rng) =>
      streamOfSizes(maxSize, n)
        .map(i => forAll(g.forSize(i), name)(p))
        .map(prop =>
          Prop { (mazSize, _, rng) =>
            prop.run(mazSize, 1, rng)
        })
        .toList
        .reduce(_ && _)
        .run(maxSize, n, rng)
    }

  def forAll[A](g: Gen[A], name: String)(p: A => Boolean): Prop = Prop {
    (n, _, rng) =>
      randomStream(g, rng) zip Stream.from(0) take n map {
        case (a, i) =>
          try {
            if (p(a)) Passed
            else Falsified(name, a.toString, i)
          } catch {
            case e: Exception => Falsified(name, buildMsg(a, e), i)
          }
      } find (_.isFalsified) getOrElse Passed
  }

  def streamOfSizes(maxSize: MaxSize, n: TestCases): Stream[Int] = {
    val step = maxSize / n.toDouble
    Stream
      .unfold(0) { i =>
        if (i > n) None
        else Some((Math.round(i * step).toInt, i + 1))
      }
      .take(n)
  }

  def buildMsg[A](a: A, e: Exception): String =
    s"test case: $a\n" +
      s"generated an exception: $e\n" +
      s"stack trace:\n${e.getStackTrace.mkString("\n")}"

  def randomStream[A](g: Gen[A], rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample(rng)))

  def forAll[A](g: Gen[A])(p: A => Boolean): Prop = forAll(g, "")(p)
}
