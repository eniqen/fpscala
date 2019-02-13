package org.bitbucket.applicative_traversable_functor

import cats.Functor

/**
  * @author Mikhail Nemenko { @literal <mnemenko@alfabank.ru>}
  */
object ApplicativeApp extends App {


}

trait Applicative[F[_]] extends Functor[F] {
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def unit[A](a: => A): F[A]

   def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa)
}
