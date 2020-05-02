package chapter6

case class State[S, +A](f: S => (A, S)) {
  def apply(s: S): (A, S) = f(s)
}

object State {
  implicit def fToState[S, A](f: S => (A, S)): State[S, A] =
    State(f)
}
