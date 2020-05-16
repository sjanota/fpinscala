import chapter5.Stream
import chapter6.RNG
import chapter8.Prop.{Falsified, Passed}

package object chapter8 {
  def forAll[A](g: Gen[A])(p: A => Boolean): Prop = Prop { (n, rng) =>
    randomStream(g, rng) zip Stream.from(0) take n map {
      case (a, i) =>
        try {
          if (p(a)) Passed
          else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
    } find (_.isFalsified) getOrElse Passed
  }

  def buildMsg[A](a: A, e: Exception): String =
    s"test case: $a\n" +
      s"generated an exception: $e\n" +
      s"stack trace:\n${e.getStackTrace.mkString("\n")}"

  def randomStream[A](g: Gen[A], rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample(rng)))
}
