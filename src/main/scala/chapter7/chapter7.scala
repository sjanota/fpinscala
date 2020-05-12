import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

package object chapter7 {

  sealed trait Future[A] {
    private[chapter7] def apply(k: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  implicit class JoinPar[A](a: Par[Par[A]]) {
    def join: Par[A] =
      ec =>
        new Future[A] {
          override private[chapter7] def apply(k: A => Unit): Unit =
            a(ec)(_(ec)(k))
      }
  }

  implicit class ParOps[A](a: Par[A]) {
    def run(s: ExecutorService): A = {
      val ref = new AtomicReference[A]()
      val latch = new CountDownLatch(1)
      a(s) { a =>
        ref.set(a); latch.countDown()
      }
      latch.await()
      ref.get()
    }

    def equal(s: ExecutorService)(other: Par[A]): Boolean =
      a.run(s) == other.run(s)

    def map[B](f: A => B): Par[B] =
      a.map2(Par.unit(()))((aa, _) => f(aa))

    def map3[B, C, D](b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] =
      map2(b)((aa, bb) => f(aa, bb, _)).map2(c)(_(_))

    def map4[B, C, D, E](b: Par[B], c: Par[C], d: Par[D])(
        f: (A, B, C, D) => E): Par[E] =
      map2(b)((aa, bb) => f(aa, bb, _, _))
        .map2(c)((fc, cc) => fc(cc, _))
        .map2(d)(_(_))

    def map5[B, C, D, E, F](b: Par[B], c: Par[C], d: Par[D], e: Par[E])(
        f: (A, B, C, D, E) => F): Par[F] =
      map2(b)((aa, bb) => f(aa, bb, _, _, _))
        .map2(c)((fc, cc) => fc(cc, _, _))
        .map2(d)((fd, dd) => fd(dd, _))
        .map2(e)(_(_))

    def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] =
      ec =>
        new Future[C] {
          override private[chapter7] def apply(k: C => Unit): Unit = {
            var ar = Option.empty[A]
            var br = Option.empty[B]

            val combiner = Actor[Either[A, B]](ec) {
              case Left(aa) =>
                br match {
                  case None     => ar = Some(aa)
                  case Some(bb) => k(f(aa, bb))
                }

              case Right(bb) =>
                ar match {
                  case None     => br = Some(bb)
                  case Some(aa) => k(f(aa, bb))
                }
            }

            a(ec)(combiner ! Left(_))
            b(ec)(combiner ! Right(_))
          }
      }

    def flatMap[B](f: A => Par[B]): Par[B] =
      map(f).join
  }

  object Par {

    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
      sequence(ps.map(asyncF(f)))

    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
      ps.foldRight(Par.unit(List.empty[A])) { (p, acc) =>
        acc.map2(p)(_ :+ _)
      }

    def foldPar[A, B](z: B, xs: IndexedSeq[A])(f: (A, B) => B)(
        m: (B, B) => B): Par[B] =
      if (xs.size <= 1)
        Par.unit(xs.headOption map (f(_, z)) getOrElse z)
      else {
        val (l, r) = xs.splitAt(xs.size / 2)
        Par.fork(foldPar(z, l)(f)(m)).map2(Par.fork(foldPar(z, r)(f)(m)))(m)
      }

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def unit[A](a: A): Par[A] =
      _ =>
        new Future[A] {
          def apply(k: A => Unit): Unit = k(a)
      }

    def fork[A](a: => Par[A]): Par[A] =
      es =>
        new Future[A] {
          override private[chapter7] def apply(k: A => Unit): Unit =
            eval(es)(a(es)(k))
      }

    def choice[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      p flatMap (if (_) t else f)

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      n flatMap choices

    def choiceMap[K, V](n: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      n flatMap choices

    private def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] {
        override def call(): Unit = r
      })

  }

}
