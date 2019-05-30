package org.bitbucket.applicative_traversable_functor

import cats.Functor

/**
  * @author Mikhail Nemenko { @literal <mnemenko@alfabank.ru>}
  */
object ApplicativeApp extends App {


}

trait Applicative[F[_]] extends Functor[F] {

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_(_))
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)(f.curried))(fb)
  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))
  def mapViaApply[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = as.foldRight(unit(List[B]()))((v, acc) => map2(f(v), acc)(_ :: _))
  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((a, b) => a -> b)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = apply(map2(fa, fb)(f.curried(_)(_)))(fc)
  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = apply(map3(fa, fb, fc)(f.curried(_)(_)(_)))(fd)
}


trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))
}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({
  type f[x] = scala.Either[E, x]
})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)
  }
}