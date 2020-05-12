package chapter7

object examples {
  def sum(xs: IndexedSeq[Int]): Par[Int] =
    Par.foldPar(0, xs)(_ + _)(_ + _)

  def max(xs: IndexedSeq[Int]): Par[Option[Int]] =
    Par.foldPar(Option.empty[Int], xs)((a, b) =>
      b map (Math.max(a, _)) orElse Some(a)) { (l, r) =>
      for { ll <- l; rr <- r } yield Math.max(ll, rr)
    }

  def countWords(ps: List[String]): Par[Int] =
    Par.foldPar(0, ps.toIndexedSeq)(_.split(" ").length + _)(_ + _)

  def sortPar(l: Par[List[Int]]): Par[List[Int]] =
    l.map(_.sorted)

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    Par.parMap(as)(a => if (f(a)) List(a) else List.empty[A]).map(_.flatten)
}
