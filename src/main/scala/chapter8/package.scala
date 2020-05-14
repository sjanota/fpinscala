package object chapter8 {
  def forAll[A](g: Gen[A])(p: A => Boolean): Prop = ???
}
