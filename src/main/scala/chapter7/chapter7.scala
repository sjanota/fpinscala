import java.util.concurrent.{ExecutorService, Future, TimeUnit}

import scala.concurrent.duration.Duration

package object chapter7 {
  type Par[A] = ExecutorService => Future[A]

  implicit class ParOps[A](a: Par[A]) {
    def run(s: ExecutorService): Future[A] = a(s)

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
