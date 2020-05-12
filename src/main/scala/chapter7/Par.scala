package chapter7

import java.util.concurrent.{ExecutorService, Future}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def unit[A](a: A): Par[A] = ???

  def fork[A](a: => Par[A]): Par[A] = ???

}
