package org.bitbucket.parallelism

import java.util.concurrent.{ExecutorService, Future, TimeUnit}

/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */
class ParallelismApp extends App {
  def sum(ints: Seq[Int]): Int = ints.foldLeft(0)((acc, v) => acc + v)

  def sum2(ints: IndexedSeq[Int]): Int = {
    if (ints.size <= 1) {
      ints.headOption.getOrElse(0)
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum2(l) + sum(r)
    }
  }

  object Par {
    type Par[A] = ExecutorService => Future[A]
    def unit[A](a: A): Par[A] = ???
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
    def run[A](ec: ExecutorService)(p: Par[A]): Future[A] = p(ec)
    def map2[A, B, C](l: Par[A], r: Par[B])(f: (A, B) => C): Par[C] = { es =>
      val lRes = l(es)
      val rRes = r(es)
      UnitFuture(f(lRes.get(), rRes.get()))
    }
    def fork[A](a: => Par[A]): Par[A] = { es =>
      es.submit(() => a(es).get())
    }

    def sequence[A](ps: List[Par[A]]): Par[List[A]] = ???
    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = ???
    def choiceN[A](n: Par[Int])(choises: List[Par[A]]): Par[A] = ???
    def choiceMap[K, V](key: Par[K])(choises: Map[K, Par[V]]): Par[V] = ???
    def chooser[A, B](pa: Par[A])(choises: A => Par[B]): Par[B] = ???

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }


    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true
      def get(timeout: Long, units: TimeUnit) = get
      def isCancelled = false
      def cancel(evenIfRunning: Boolean): Boolean = false
    }
  }

  import Par._

  def sum3(ints: IndexedSeq[Int]): Int = {
    import Par._
    if (ints.size <= 1) {
      ints.headOption.getOrElse(0)
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val sumL = unit(sum3(l))
      val sumR = unit(sum3(r))
      run(sumL) + run(sumR)
    }
  }

  def sum4(ints: IndexedSeq[Int]): Par[Int] = {
    import Par._
    if (ints.size <= 1) {
      Par.unit(ints.headOption.getOrElse(0))
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(sum4(l), sum4(r))(_ + _)
    }
  }

  def sum5(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 1) {
      Par.unit(ints.headOption.getOrElse(0))
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum5(l)), Par.fork(sum5(r)))(_ + _)
    }
  }
}
