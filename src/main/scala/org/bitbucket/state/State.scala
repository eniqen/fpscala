package org.bitbucket.state

/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */
case class State[S, +A](run: S => (A, S)) {
  import State._
  def map[B](f: A => B): State[S, B] = flatMap(s => unit(f(s)))
  def map2[B, C](second: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => second.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, newState) = run(s)
      f(a).run(newState)
    })
}

object State {
  import scala.collection.immutable.List

  def unit[S, A](v: A): State[S, A] = State(run => v -> run)
  def sequence[S, A](list: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, l: List[State[S, A]], acc: List[A]): (List[A], S) = l match {
      case Nil => acc.reverse -> s
      case h :: t =>
        val (v, newState) = h.run(s)
        go(newState, t, v +: acc)
    }
    State(run => go(run, list, List.empty[A]))
  }
}
