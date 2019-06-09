package org.bitbucket.applicative_traversable_functor

import java.util.Date

import cats.Functor
import org.bitbucket.applicative_traversable_functor.WebForm.{validBirthdate, validName, validPhone}

/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */
object ApplicativeApp extends App {

  println(Monad.eitherMonad[Int].unit(5).map(_ + 3))

  def validWebForm(name: String, birthdate: String,
                   phone: String): Validation[String, WebForm] = {

    Applicative.validateApplicative[String].map3(
      validName(name),
      validBirthdate(birthdate),
      validPhone(phone))(
      WebForm(_,_,_))

  }

  println(validWebForm("", "jjjjjjjj", "111rgrg"))
}

trait Applicative[F[_]] extends Functor[F] { self =>

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


  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = new Applicative[({
  type f[x] = (F[x], G[x])
})#f] {
    override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

    override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) = (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = new Applicative[({
  type f[x] = F[G[x]]
})#f] {
    override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
  }

}


trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))

  def compose[G[_]](G: Monad[G]): Monad[({type f[x] = F[G[x]]})#f] = ???

}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({
  type f[x] = scala.Either[E, x]
})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)
  }
}


case class WebForm(name: String, birthdate: Date, phoneNumber: String)


sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

object WebForm {
  def validName(name: String): Validation[String, String] = if (name != "") Success(name)
  else Failure("Name cannot be empty")
  def validBirthdate(birthdate: String): Validation[String, Date] = try {
    import java.text._
    Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate))
  } catch {
    case _: Throwable => Failure("Birthdate must be in the form yyyy-MM-dd")
  }
  def validPhone(phoneNumber: String): Validation[String, String] = if (phoneNumber.matches("[0-9]{10}"))
    Success(phoneNumber)
  else Failure("Phone number must be 10 digits")
}

object Applicative {
  implicit def validateApplicative[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
      case (Success(a), Success(b)) => Success(f(a, b))
      case (Failure(ah, at), Failure(bh, ht)) => Failure(ah, at ++ Vector(bh) ++ ht)
      case (e @ Failure(_, _), _) => e
      case (_, e @ Failure(_, _)) => e
    }
  }
}

