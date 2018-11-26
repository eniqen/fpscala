/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */
object Tree extends App {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + depth(left) max depth(right)
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A,B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

   def sizeViaFold[A](tree: Tree[A]): Int = fold(tree)(a => 1)(1 + _ + _)
   def maximumViaFold(tree: Tree[Int]): Int = fold(tree)(identity)(_ max _)
   def depthViaFold[A](tree: Tree[A]): Int = fold(tree)(_ => 0)(_ max _)
   def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)) : Tree[B])(Branch(_, _))

  val tree = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Branch(Leaf(4), Branch(Leaf(5), Leaf(6))))
  println(size(tree))
  println(maximum(tree))
  println(depth(tree))
  println(map(tree)(_ + 1))
}

sealed trait Tree[A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

