def isSorted[A](as:Array[A])(ordered: (A, A) => Boolean): Boolean = {
  def loop(n: Int): Boolean = {
    if(as.length - 1 >= n) true
    else if (!ordered(as(n), as(n + 1))) false
    else loop(n + 1)
  }
  loop(0)
}

isSorted(Array(1,2,3,4))(_ > _)
isSorted(Array(1,2,3,4, 5))(_ > _)
isSorted(Array(1,2,3,4, 6))(_ < _)


def partial1[A,B,C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

val f = (x: Int, y: Int) => (x + y).toString

partial1(5, f)(6)