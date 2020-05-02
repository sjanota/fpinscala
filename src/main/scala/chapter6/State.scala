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

  def modify(f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  def get: State[S, S] =
    State(s => (s, s))

  def set(s: S): State[S, Unit] =
    State(_ => ((), s))

}

object State {

  def both[S, A, B](ra: State[S, A], rb: State[S, B]): State[S, (A, B)] =
    ra.map2(rb)((_, _))

  def sequence[S, A](rs: List[State[S, A]]): State[S, List[A]] =
    rs.foldLeft(unit[S, List[A]](List())) { (acc, r) =>
      r.map2(acc)(_ :: _)
    }

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
}
