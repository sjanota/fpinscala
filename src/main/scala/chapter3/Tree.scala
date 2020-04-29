package chapter3

sealed trait Tree[+A] {
  def size: Int = fold(_ => 1)(1 + _ + _)

  def depth: Int = fold(_ => 0)((l, r) => 1 + (l max r))

  def map[B](f: A => B): Tree[B] = fold(v => Leaf(f(v)): Tree[B])(Branch(_, _))

  def fold[B](z: A => B)(m: (B, B) => B): B = this match {
    case Leaf(value)         => z(value)
    case Branch(left, right) => m(left.fold(z)(m), right.fold(z)(m))
  }
}

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  implicit class IntTree(t: Tree[Int]) {
    def maximum: Int = t.fold(v => v)(_ max _)
  }

}
