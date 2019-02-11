

def curry[A,B,C](f: (A, B) => C): A => B => C = (a: A) => (b: B) => f(a, b)

val f = (x: Int, y: Int) => x + y

curry(f)(5)(6)


def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)


def g(a: Int)(b: Int) = a + b

uncurry(g)(5, 5)

def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

val x: Int => Int = _ + 5
val y: Int => Int = _ * 10

compose(x, y)(10)