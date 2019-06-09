package org.bitbucket.monads

import javax.swing.text.html.parser.Parser

/**
  * @author Mikhail Nemenko { @literal <mnemenko@alfabank.ru>}
  */
trait Monad[F[_]] {
  def unit[A](a: => A): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))
}


object Monad {
//  val parMonad: Monad[Par] = new Monad[Par] {
//    override def unit[A](a: => A): Par[A] = ???
//    override def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] = ???
//  }

//  val parserMonad: Monad[Parser] = new Monad[Parser] {
//    override def unit[A](a: => A): Parser[A] = ???
//    override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = ???
//  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = ???
  }

  def sequence[F[_], A](lma: List[F[A]]): F[List[A]] = ???
  def traverse[F[_], A, B](la: List[A])(f: A => F[B]): F[List[B]] = ???
  def replicateM[F[_], A](n: Int, ma: F[A]): F[List[A]] = ???
  def filterM[F[_], A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ???
  def compose[F[_], A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = ???
  def flatMapViaCompose[F[_], A, B](fa: F[A])(f: A => F[B]): F[B] = ???
  def join[F[_], A](mma: F[F[A]]): F[A] = ???
  def flatMapViaJoin[F[_], A, B](fa: F[A])(f: A => F[B]): F[B] = ???
  def composeViaJoin[F[_], A, B](fa: F[A])(f: A => F[B]): F[B] = ???
}