package org.bitbucket.state

/**
  * @author Mikhail Nemenko { @literal <nemenkoma@gmail.com>}
  */
object CandyMachine extends App {
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def updateState(input: Input, machine: Machine): Machine =
    (input, machine) match {
      case (_, s @ Machine(_, 0, _))        => s
      case (Turn, s @ Machine(true, _, _))  => s
      case (Coin, s @ Machine(false, _, _)) => s
      case (Coin, Machine(true, candies, coins)) =>
        machine.copy(locked = false, candies = candies, coins = coins + 1)
      case (Turn, Machine(false, candies, coins)) =>
        machine.copy(locked = true, candies - 1, coins)
    }

  import State._
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs.map(input => modify[Machine](updateState(input, _))))
      s <- get
    } yield (s.coins, s.candies)

  def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] = {
    sequence(inputs.map(input => modify[Machine](updateState(input, _))))
      .flatMap(_ => get[Machine].map(m => m.candies -> m.coins))
  }

  println(
    simulateMachine2(List(Turn, Coin, Turn, Coin, Turn))
      .run(Machine(true, 10, 0))
      ._1)
}
