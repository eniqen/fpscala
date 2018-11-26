package lazyness

import scala.annotation.tailrec

/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */
object StreamApp extends App {
  import Stream._
  private val stream: Stream[Int] =
    cons(5, cons(6, cons(7, cons(8, cons(9, empty)))))
  println(stream.toList)
  println(stream.take(3).toList)
  println(stream.drop(0).toList)
  println(stream.exist(_ == 6))
  println(stream.foldRight(0)(_ + _))
  println(stream.forAll(_ < 10))
  println(stream.takeWhileViaFoldRight(_ < 7).toList)
  println(stream.headOptionViaFoldRight)
  println(stream.map(_ + 2).toList)
  println(stream.filter(_ % 2 == 0).toList)
  println(stream.append(stream).toList)
  println(stream.flatMap(x => cons(x + 5, cons(x + 6, empty))).toList)
  println(fibs.take(7).toList.last)
}

sealed trait Stream[+A] {
  import Stream._
  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, _) => Some(h())
  }

  @tailrec
  final def exist(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exist(p)
    case Empty      => false
  }

  def toList: List[A] = this match {
    case Empty      => Nil
    case Cons(h, t) => h() +: t().toList
  }

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
      case _                   => empty
    }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _                    => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, z) => p(a) && z)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty[A])

  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))
  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)
  def append[B >: A](f: => Stream[B]): Stream[B] =
    foldRight(f)((a, b) => cons(a, b))
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))
}
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
case object Empty extends Stream[Nothing]

object Stream {
  def empty[A]: Stream[A] = Empty
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def ones: Stream[Int] = cons(1, ones)
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))
  def fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))
    go(0, 1)
  }
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
