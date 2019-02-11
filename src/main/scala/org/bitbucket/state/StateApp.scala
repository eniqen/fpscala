package org.bitbucket.state
import scala.collection.immutable.List

/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */
object StateApp extends App {

  object RNG {
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (number, rng2) = rng.next
      (math.abs(number % Int.MaxValue) + 1, rng2)
    }

    def double(rng: RNG): (Double, RNG) = {
      val (number, rng2) = rng.next
      (1 / (number * (Int.MaxValue.toDouble + 1)), rng2)
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (intValue, rng2) = rng.next
      val (doubleValue, rng3) = nonNegativeInt(rng2)
      ((intValue, doubleValue), rng3)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val (result, rng2) = intDouble(rng)
      (result.swap, rng2)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, rng2) = rng.next
      val (d2, rng3) = rng2.next
      val (d3, rng4) = rng3.next
      ((d1, d2, d3), rng4)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      def go(count: Int)(acc: List[Int], next: RNG): (List[Int], RNG) =
        count match {
          case v if v > 0 =>
            val (res, rng2) = next.next
            go(count - 1)(res :: acc, rng2)
          case _ => acc -> next
        }
      go(count)(List.empty[Int], rng)
    }

    type Rand[+A] = RNG => (A, RNG)

    object Rand {
      val int: Rand[Int] = _.next

      def unit[A](a: A): Rand[A] = rng => (a, rng)

      def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
        val (res, rng2) = s(rng)
        (f(res), rng2)
      }

      def noneNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

      def double: Rand[Double] =
        map(nonNegativeInt)(i => 1 / (i * (Int.MaxValue.toDouble + 1)))

      def map2[A, B, C](first: Rand[A], second: Rand[B])(
          f: (A, B) => C): Rand[C] = rng => {
        val (v1, rng1) = first(rng)
        val (v2, rng2) = second(rng1)
        f(v1, v2) -> rng2
      }

      def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
        map2(ra, rb)((_, _))

      val randIntDouble: Rand[(Int, Double)] = both(int, double)
      val randDoubleInt: Rand[(Double, Int)] = both(double, int)

      def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
        fs.foldRight(unit(List.empty[A])) {
          case (v, acc) => map2(v, acc)(_ :: _)
        }

      def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = rng => {
        val (v, rng2) = r(rng)
        f(v)(rng2)
      }

      def nonNegativeLessThan(v: Int): Rand[Int] = {
        flatMap(nonNegativeInt)(i => {
          val mod = i % v
          if (i + (v - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(v)
        })
      }

      def mapViaFlatmap[A, B](v: Rand[A])(f: A => B): Rand[B] = {
        flatMap(v)(a => unit(f(a)))
      }

      def map2ViaFlatmap[A, B, C](first: Rand[A], second: Rand[B])(
          f: (A, B) => C): Rand[C] = {
        flatMap(first)(a => map(second)(b => f(a, b)))
      }
    }
  }
}

trait RNG {
  def next: (Int, RNG)
}
