import java.util.concurrent.{ExecutorService, Future, TimeUnit}

import scala.concurrent.duration.Duration

package object chapter7 {
  type Par[A] = ExecutorService => Future[A]

  implicit class ParOps[A](a: Par[A]) {
    def run(s: ExecutorService): Future[A] = a(s)

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
      ec => {
        val af = a(ec)
        val bf = b(ec)
        new Future[C] {
          override def cancel(mayInterruptIfRunning: Boolean): Boolean =
            af.cancel(mayInterruptIfRunning) && bf.cancel(mayInterruptIfRunning)

          override def isCancelled: Boolean =
            af.isCancelled || bf.isCancelled

          override def isDone: Boolean =
            af.isDone && bf.isDone

          override def get(): C =
            f(af.get, bf.get)

          override def get(timeout: Long, unit: TimeUnit): C = {
            val start = System.nanoTime()
            val ag = af.get(timeout, unit)
            val end = System.nanoTime()
            val d = Duration(timeout, unit) - Duration(end - start,
                                                       TimeUnit.NANOSECONDS)
            val bg = bf.get(d.length, d.unit)
            f(ag, bg)
          }
        }
      }
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
      _ => UnitFuture(a)

    def fork[A](a: => Par[A]): Par[A] =
      ec => ec.submit(() => a(ec).get)

    private case class UnitFuture[A](get: A) extends Future[A] {
      override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

      override def isCancelled: Boolean = false

      override def isDone: Boolean = true

      override def get(timeout: Long, unit: TimeUnit): A = get
    }

  }

}
