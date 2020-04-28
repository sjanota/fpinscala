package chapter2

object Chapter2Exercises {
  def fib(n: Int): Int = {
    @scala.annotation.tailrec
    def go(n0: Int, n1: Int, i: Int): Int = {
      val next = n0 + n1
      if (i == n) next
      else go(n1, next, i + 1)
    }

    n match {
      case 1 => 0
      case 2 => 1
      case _ => go(0, 1, 3)
    }
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @scala.annotation.tailrec
    def go(n: Int): Boolean =
      if (n >= as.length) true
      else if (!ordered(as(n - 1), as(n))) false
      else go(n + 1)

    if (as.length < 2) true
    else go(1)
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}
