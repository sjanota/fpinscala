package object chapter3 {

  sealed trait List[+A] {
    def drop(n: Int): Option[List[A]] = this match {
      case Nil        => None
      case Cons(h, t) => if (n == 0) Some(this) else t.drop(n - 1)
    }

    def dropWhile(p: A => Boolean): Option[List[A]] = this match {
      case Nil        => None
      case Cons(h, t) => if (p(h)) t.dropWhile(p) else Some(this)
    }

    def tail: Option[List[A]] = drop(1)
    def +:[B >: A](b: B): List[B] = Cons(b, this)
    def setHead[B >: A](b: B): Option[List[B]] = tail.map(Cons(b, _))
  }
  case object Nil extends List[Nothing]
  case class Cons[+A](h: A, t: List[A]) extends List[A]

  object List {
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(reverse(as), z)((b, a) => f(a, b))

    def sum(ints: List[Int]): Int =
      foldLeft(ints, 0)(_ + _)

    def product(doubles: List[Double]): Double =
      foldLeft(doubles, 1.0)(_ * _)

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def length[A](as: List[A]): Int =
      foldLeft(as, 0)((acc, _) => acc + 1)

    @scala.annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil        => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

    def reverse[A](as: List[A]): List[A] =
      foldLeft(as, Nil: List[A])((t, h) => Cons(h, t))

    def init[A](as: List[A]): List[A] = {
      @scala.annotation.tailrec
      def go(acc: List[A], rest: List[A]): List[A] = rest match {
        case Nil          => sys.error("cannot have init of Nil")
        case Cons(_, Nil) => reverse(acc)
        case Cons(h, t)   => go(Cons(h, acc), t)
      }

      go(Nil, as)
    }

    def append[A](as1: List[A], as2: List[A]): List[A] =
      foldRight(as1, as2)(Cons(_, _))

    def concat[A](ls: List[List[A]]) = foldRight(ls, Nil: List[A])(append)

    def addOne(ints: List[Int]): List[Int] =
      foldRight(ints, Nil: List[Int])((x, acc) => Cons(x + 1, acc))

    def d2string(l: List[Double]): List[String] =
      foldRight(l, Nil: List[String])((x, acc) => Cons(x.toString, acc))

    def map[A, B](l: List[A])(f: A => B): List[B] =
      foldRight(l, Nil: List[B])((x, acc) => Cons(f(x), acc))

    def filter[A](l: List[A])(p: A => Boolean): List[A] =
      flatMap(l)(x => if (p(x)) List(x) else Nil)

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
      foldRight(l, Nil: List[B])((x, acc) => append(f(x), acc))

    def addLists(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
      case (Nil, _)                   => Nil
      case (_, Nil)                   => Nil
      case (Cons(x, t1), Cons(y, t2)) => Cons(x + y, addLists(t1, t2))
    }

    def zipWith[A, B](l1: List[A], l2: List[A])(f: (A, A) => B): List[B] =
      (l1, l2) match {
        case (Nil, _)                   => Nil
        case (_, Nil)                   => Nil
        case (Cons(x, t1), Cons(y, t2)) => Cons(f(x, y), zipWith(t1, t2)(f))
      }
  }

  def exercise1: Int = {
    List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _)))            => x
      case Nil                                     => 42
      case Cons(x, Cons(y, (Cons(3, Cons(4, _))))) => x + y
      case Cons(h, t)                              => h + List.sum(t)
      case _                                       => 101
    }
  }

  def exercise8: List[Int] =
    List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))

  def exercise10: Int =
    List.foldLeft(List(1, 2, 3), 1)(_ * _)

  def exercise12 = List.reverse(List(1, 2, 3))
  def exercise14 = List.append(List(3, 2, 1), List(6, 5, 4))

  def exercise22 = List.addLists(List(1, 2, 3), List(4, 5, 6))
}
