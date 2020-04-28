package chapter2

object MyModule {
  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))
  }

  private def formatAbs(x: Int) = format("abs", x, abs)

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def format(name: String, x: Int, f: Int => Int) =
    "The %s value of %d is %d".format(name, x, f(x))

  private def formatFactorial(x: Int) = format("factorial", x, factorial)

  def factorial(n: Int): Int = {
    @scala.annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, acc * n)

    go(n, 1)
  }

  def findFirst[A](ss: Array[A], p: A => Boolean): Int = {
    @scala.annotation.tailrec
    def go(n: Int): Int =
      if (n >= ss.length) -1
      else if (p(ss(n))) n
      else go(n + 1)

    go(0)
  }
}
