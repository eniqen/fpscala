import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }
  def setHead[A](list: List[A], newHead: A): List[A] = list match {
    case Nil         => Nil
    case Cons(_, xs) => Cons(newHead, xs)
  }

  @tailrec
  def drop[A](list: List[A], n: Int): List[A] = list match {
    case Nil => Nil
    case xss @ Cons(_, _) => if (n <= 0) xss else drop(tail(xss), n - 1)
  }

  @tailrec
  def dropWhile[A](list: List[A], f: A => Boolean): List[A] = list match {
    case Nil => Nil
    case xss @ Cons(x, xs) => if (f(x)) dropWhile(xs, f) else xss
  }
  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }
  def init[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }
  def foldRight[A, B](list: List[A], acc: B)(f: (A, B) => B): B = list match {
    case Nil => acc
    case Cons(x, xs) => f(x, foldRight(xs, acc)(f))
  }
  def foldRight2[A, B](list: List[A], acc: B)(f: (A, B) => B): B = list match {
    case Nil => acc
    case Cons(x, xs) => foldRight2(xs, f(x, acc))(f)
  }
  def foldLeft[A, B](list: List[A], z: B)(f: (B, A) => B) : B = list match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }
  def sumViaFoldRight(list: List[Int]): Int = foldRight(list, 0)(_ + _)
  def productViaFoldRight(list: List[Double]): Double = foldRight(list, 1.0)(_ * _)
  def lengthViaFoldRight[A](list: List[A]): Int = foldRight(list, 0)((_, b) => b + 1)
  def sumViaFoldLeft(list: List[Int]): Int = foldLeft(list, 0)(_ + _)
  def productViaFoldLeft(list: List[Double]): Double = foldLeft(list, 1.0)(_ * _)
  def lengthViaFoldLeft[A](list: List[A]): Int = foldLeft(list, 0)((acc, _) => acc + 1)
  def reverse[A](list: List[A]): List[A] = foldLeft(list, Nil: List[A])((acc , a) => Cons(a, acc))
  def foldRightViaFoldLeft[A, B](list: List[A], z: B)(f: (A, B) => B): B = foldLeft(list, z)((acc, a) => f(a, acc))
  def foldLeftViaFoldRight[A, B](list: List[A], z: B)(f: (B, A) => B): B = foldRight(list, z)((a, acc) => f(acc, a))
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)(Cons(_, _))
  def concat[A](list: List[List[A]]): List[A] = foldRight(list, Nil:List[A])(append)
  def map[A, B](list: List[A])(f: A => B): List[B] = list match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def filterViaFoldRight[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((a, acc) => if(f(a)) Cons(a, acc) else acc)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def zipWith[A, B](list: List[A], list2: List[A])(f: (A, A) => B): List[B] = (list, list2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1, x2), zipWith(xs1, xs2)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(x, xs), Cons(x2, xs2)) =>
      if (x == x2) hasSubsequence(xs, xs2)
      else hasSubsequence(xs, sub)
  }
}

val x = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
val fInc: Int => Int = _ + 1
val fToString: Int => String = _.toString

List.append(x, Nil)

//List.foldRight(x, Nil: List[Int])(Cons(_, _))
//List.reverse(x)
//
List.foldLeft(x, 0)(_ - _)
List.foldLeftViaFoldRight(x, 0)(_ - _)

List.foldRight(x, 0)(_ - _)
List.foldRightViaFoldLeft(x, 0)(_ - _)

List.map(x)(fInc)
List.map(x)(fToString)

List.flatMap(List(1,2,3,4))(x => List(x, x))

List.zipWith(x, x)(_ + _)

List.hasSubsequence(Nil, List(6))

