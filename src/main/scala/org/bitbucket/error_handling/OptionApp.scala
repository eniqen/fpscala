package org.bitbucket.error_handling

/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */

object OptionApp extends App {

}


object Option {
  def apply[A](value: A): Option[A] = Some(value)
}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case None    => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(v) if v => this
    case _            => None
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(v) => v
    case None    => default
  }
  def orElse[B >: A](default: => Option[B]): Option[B] = this match {
    case x @ Some(_) => x
    case None        => default
  }

  def map2[A, B, C](x: Option[A], y: Option[B])(f: (A, B) => C): Option[C] =
    x.flatMap(a => y.map(b => f(a, b)))

  def sequence[A](
      a: scala.collection.immutable.List[Option[A]]): Option[List[A]] =
    a match {
      case scala.collection.immutable.Nil => Some(Nil)
      case h :: t                         => h flatMap (hh => sequence(t) map (hh :: _))
    }

  def traverse[A, B](l: List[A])(f: A => Option[B]): Option[List[B]] = l match {
    case Nil    => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }
}

case class Some[A](get: A) extends Option[A]

case object None extends Option[Nothing]
