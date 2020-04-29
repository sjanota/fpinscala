package chapter4

sealed trait Option[+A] {
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  def map[B](f: A => B): Option[B] = this match {
    case None        => None
    case Some(value) => Some(f(value))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None        => default
    case Some(value) => value
  }

  def filter(p: A => Boolean): Option[A] =
    flatMap(v => if (p(v)) Some(v) else None)

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)
}

case object None extends Option[Nothing]

case class Some[A](value: A) extends Option[A]

object Option {
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def sequence[A](l: List[Option[A]]): Option[List[A]] =
    traverse(l)(identity)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil)) { (aa, acc) =>
      map2(f(aa), acc)(_ :: _)
    }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))
}
