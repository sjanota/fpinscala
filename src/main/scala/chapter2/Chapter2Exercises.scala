package chapter2

object Chapter2Exercises {
  def fib(n: Int): Int = {
    @scala.annotation.tailrec
    def go(n0: Int, n1: Int, i: Int): Int = {
      val next = n0 + n1
      if (i == n) next
      else go(n1, next, i+1)
    }

    n match {
      case 1 => 0
      case 2 => 1
      case _ => go(0, 1, 3)
    }
  }
}
