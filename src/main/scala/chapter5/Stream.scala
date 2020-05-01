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

  def take(n: Int): Stream[A] =
    unfold((n, this)) {
      case (0, _) => Option.empty
      case (i, s) => s.headOption.map(h => (h, (i - 1, s.drop(1))))
    }

  @scala.annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    unfold(this) { s =>
      s.headOption.flatMap(h =>
        if (p(h)) Option(h, s.drop(1)) else Option.empty)
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((h, b) => p(h) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, b) => p(h) && b)

  def map[B](f: A => B): Stream[B] =
    unfold(this) { s =>
      s.headOption map (h => (f(h), s.drop(1)))
    }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)

  def append[B >: A](as: => Stream[B]): Stream[B] =
    foldRight(as)((a, b) => cons(a, b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption
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

  val fibs: Stream[Int] =
    unfold((0, 1))(s => Option((s._1, (s._2, s._1 + s._2))))

  def constant[A](a: A): Stream[A] =
    unfold(a)(_ => Option((a, a)))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def next(s: S): Stream[A] =
      f(s).map(r => cons(r._1, next(r._2))) getOrElse empty[A]

    next(z)
  }

  def from(n: Int): Stream[Int] =
    unfold(n)(s => Option((s, s + 1)))

}
