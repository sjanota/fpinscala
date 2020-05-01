package chapter5

import chapter5.Stream._

sealed trait Stream[+A] {
  def headOption: Option[A] =
    foldRight(Option.empty[A])((h, _) => Option(h))

  def toList: List[A] = {
    val buffer = new collection.mutable.ListBuffer[A]

    @scala.annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buffer += h()
        go(t())
      case _ => buffer.toList
    }

    go(this)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _                    => empty
  }

  @scala.annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, b) => if (p(h)) cons(h, b) else empty)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((h, b) => p(h) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, b) => p(h) && b)

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)

  def append[B >: A](as: => Stream[B]): Stream[B] =
    foldRight(as)((a, b) => cons(a, b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)
}

case object Empty extends Stream[Nothing]

case class Cons[A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
