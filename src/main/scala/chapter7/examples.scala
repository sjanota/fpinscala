package chapter7

object examples {
  def sum(xs: IndexedSeq[Int]): Par[Int] =
    if (xs.size <= 1)
      Par.unit(xs.headOption getOrElse 0)
    else {
      val (l, r) = xs.splitAt(xs.size / 2)
      Par.fork(sum(l)).map2(Par.fork(sum(r)))(_ + _)
    }

}
