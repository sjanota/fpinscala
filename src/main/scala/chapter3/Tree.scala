package chapter3

sealed trait Tree[+A] {
  def size: Int = this match {
    case Leaf(_)             => 1
    case Branch(left, right) => 1 + left.size + right.size
  }

  def depth: Int = this match {
    case Leaf(_)             => 0
    case Branch(left, right) => 1 + (left.depth max right.depth)
  }

  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(value)         => Leaf(f(value))
    case Branch(left, right) => Branch(left.map(f), right.map(f))
  }

  def fold[B](z: B)(f: (A, B) => B): B = this match {
    case Leaf(value)         => f(value, z)
    case Branch(left, right) => right.fold(left.fold(z)(f))(f)
  }
}

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  implicit class IntTree(t: Tree[Int]) {
    def maximum: Int = t match {
      case Leaf(value)         => value
      case Branch(left, right) => left.maximum max right.maximum
    }
  }

}
