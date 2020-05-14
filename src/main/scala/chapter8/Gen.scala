package chapter8

trait Gen[T]

object Gen {
  def listOf[A](g: Gen[A]): Gen[List[A]] = ???

  def choose[A](min: A, max: A): Gen[A] = ???
}
