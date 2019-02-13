package org.bitbucket.monoid

/**
  * @author Mikhail Nemenko { @literal <mnemenko@alfabank.ru>}
  */
trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb:Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
}

object Foldable {

  val foldableList: Foldable[List] = new Foldable[scala.List] {
    override def foldRight[A, B](as: scala.List[A])(z: B)(f: (A, B) => B): B = ???
    override def foldLeft[A, B](as: scala.List[A])(z: B)(f: (B, A) => B): B = ???
    override def foldMap[A, B](as: scala.List[A])(f: A => B)(mb: Monoid[B]): B = ???
  }

  val foldableIndexSec: Foldable[IndexedSeq] = new Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = ???
    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = ???
    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = ???
  }

  val foldableStream: Foldable[Stream] = new Foldable[Stream] {
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = ???
    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = ???
    override def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B = ???
  }

  import org.bitbucket.Tree
  val foldableTree: Foldable[Tree] = new Foldable[Tree] {
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = ???
    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = ???
    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = ???
  }

  val foldableOpt: Foldable[Option] = new Foldable[Option] {
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = ???
    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = ???
    override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = ???
  }

  def toList[F[_], A](fa: F[A]): List[A] = ???
}
