package org.bitbucket.monoid

import cats.kernel.Monoid

/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */
class MonoidApp extends App {

  val stringMonoid: Monoid[String] = new Monoid[String] {
    override def op(l: String, r: String): String = l + r
    override def zero: String = ""
  }

  def listMonoid[A]: Monoid[scala.List[A]] = new Monoid[List[A]] {
    override def op(l: scala.List[A], r: scala.List[A]): scala.List[A] = l ++ r
    override def zero: scala.List[A] = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(l: Int, r: Int): Int = l + r
    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(l: Int, r: Int): Int = l * r
    override def zero: Int = 0
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(l: Boolean, r: Boolean): Boolean = l | r
    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(l: Boolean, r: Boolean): Boolean = l & r
    override def zero: Boolean = false
  }

  def optionMonoid[A](implicit M: Monoid[A]): Monoid[Option[A]] =
    new Monoid[Option[A]] {
      override def op(l: Option[A], r: Option[A]): Option[A] = (l, r) match {
        case (Some(lv), Some(rv)) => Some(M.op(lv, rv))
        case (None, r @ Some(_))  => r
        case (l @ Some(_), None)  => l
        case _                    => zero
      }
      override def zero: Option[A] = None
    }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(l: A => A, r: A => A): A => A = l andThen r
    def zero: A => A = identity
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero)((acc, v) => m.op(acc, f(v)))
  }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(a => f.curried(a))(z)
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    foldMap(as, endoMonoid[B])(a => b => f(b, a))(z)
  }

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v match {
    case Seq()      => m.zero
    case h +: Seq() => f(h)
    case _          => {
      val (l, r) = v.splitAt(v.length / 2)

      m.op(foldMapV(l,m)(f), foldMapV(r, m)(f))
    }
  }

//  def par[A](m: Monoid[A]): Monoid[Par[A]]
//  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = ???

  trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(l: WC, r: WC): WC = (l, r) match {
      case (Part(l, w, r), Part(l2, w2, r2)) => Part(l, w + w2 + (if((r + l2).isEmpty) 0 else 1), r2)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Stub(c), Part(l, w, r)) => Part(l + c, w, r)
      case (Stub(lc), Stub(rc)) => Stub(lc + rc)
    }

    override def zero: WC = Stub("")
  }

  def productMonoid[A, B](lm: Monoid[A], rm: Monoid[B]): Monoid[(A, B)] = ???

  def mapMergeMonoid[K, V](V : Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def op(l: Map[K, V], r: Map[K, V]): Map[K, V] = (l.keySet ++ r.keySet).foldLeft(zero) {
      (acc, k) => acc.updated(k, V.op(l.getOrElse[V](k, V.zero), r.getOrElse[V](k, V.zero)))
    }
    override def zero: Map[K, V] =  Map[K, V]()
  }

  def functionMonoid[A, ?](B : Monoid[?]): Monoid[A => ?] = new Monoid[A => ?] {
    override def op(l: A => ?, r: A => ?): A => ? = a => B.op(l(a), r(a))
    override def zero: A => ? = _ => B.zero
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = ???

}

trait Monoid[A] {
  def op(l: A, r: A): A
  def zero: A
}


