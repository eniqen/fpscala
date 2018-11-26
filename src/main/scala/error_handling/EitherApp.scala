package error_handling

import scala.{Either => _}

/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */
object EitherApp extends App {

}

object Either {
  def left[E](v: E): Either[E, Nothing] = Left(v)
  def right[A](v: A): Either[Nothing, A] = Right(v)
}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case l @ Left(_)  => l
    case Right(value) => Right(f(value))
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case l @ Left(_)  => l
    case Right(value) => f(value)
  }
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case _       => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      t <- this
      that <- b
    } yield f(t, b)

  def sequence[E, A](l: List[Either[E, A]]): Either[E, List[A]] =
    traverse(l)(identity)

  def traverse[E, A, B](l: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    l.foldRight[Either[E, List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))
}

case class Left[+E](get: E) extends Either[E, Nothing]
case class Right[+A](get: A) extends Either[Nothing, A]
