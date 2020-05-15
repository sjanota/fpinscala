package chapter6

import chapter6.State._
case class State[S, +A](f: S => (A, S)) {
  def apply(s: S): (A, S) = f(s)

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = f(s)
      g(a)(s2)
    })

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
    for { a <- this; b <- rb } yield f(a, b)

}

object State {

  def both[S, A, B](ra: State[S, A], rb: State[S, B]): State[S, (A, B)] =
    ra.map2(rb)((_, _))

  def sequence[S, A](rs: Seq[State[S, A]]): State[S, Seq[A]] =
    traverse(rs)(identity)

  def traverse[S, A, B](rs: Seq[A])(f: A => State[S, B]): State[S, Seq[B]] =
    rs.foldRight(unit[S, List[B]](List())) { (a, acc) =>
      f(a).map2(acc)(_ :: _)
    }

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
