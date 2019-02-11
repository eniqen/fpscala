import scala.annotation.tailrec

def fib(value: Int): BigInt = {
  @tailrec
  def helper(cur: BigInt, prev: BigInt, n: BigInt): BigInt = {
    val zero = BigInt(0)
    if (cur == zero) prev
    else if (cur > 0) helper(cur - 1, n, n + prev)
    else helper(cur + 1, n, prev - n)
  }
  helper(value, 0, 1)
}

fib(-10)