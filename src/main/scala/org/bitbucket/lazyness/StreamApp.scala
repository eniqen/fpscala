package org.bitbucket.lazyness

import scala.annotation.tailrec

/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */
object StreamApp extends App {

  import scala.util.control.TailCalls
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
  println(fibs.take(7).toList)
  println(from(5).take(7).toList)
  println(fromViaUnfold(5).take(7).toList)


  println("FIB VIA UNFOLD " + fibViaUnfold.take(7).toList.last)
  println("MAP VIA UNFOLD " + stream.mapViaUnfold(_ * 10).take(5).toList)
  println("TAILS VIA UNFOLD " + stream.tails.map(_.toList).toList)
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


  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case Empty => None
  }
  def takeViaUnfold(count: Int): Stream[A] = unfold(count -> this){
    case (c, Cons(h, t)) if c >= 0 => Some(h() -> (c - 1, t()))
    case _ => None
  }
  def takeWhileViaUnfold(f : A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if f(h()) => Some(h() -> t())
    case _ => None
  }

  def zipWithViaUnfold[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()) -> (t1(), t2()))
    case _ => None
  }

  def zipAllViaUnfold[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold(this -> s2) {
    case (Empty, Empty) => None
    case (Empty, Cons(h, t)) => Some((None, Some(h())), empty -> t())
    case (Cons(h, t), Empty) => Some((Some(h()), None), t() -> empty)
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), t1() -> t2())
  }

  def startsWith[A](s: Stream[A]): Boolean = {
    zipAllViaUnfold(s) takeWhile {
      case (_, b) => b.isDefined
    } forAll {
      case (a, b) => a == b
    }
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case s @ Cons(h, t) => Some(s -> t())
    case Empty => None
  }.append(Stream(empty))
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

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case None => Empty
  }

  def fibViaUnfold: Stream[Int] = unfold((0, 1)) {
    case (a , b) => Some((a, (b, a + b)))
    case _ => None
  }

    unfold(cons(0, cons(1, empty))) {
    case Cons(h, t) => t().headOption.flatMap(v => Some(h() -> cons(v, cons(v + h(), empty))))
    case Empty => None
  }

  def constantViaUnfold[A](a: A) = unfold(a)(a => Some(a -> a))
  def fromViaUnfold(from: Int) = unfold(from)(v => Some((v, v + 1)))
  def onesViaUnfold  = unfold(1)(x => Some(x -> x))

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}


