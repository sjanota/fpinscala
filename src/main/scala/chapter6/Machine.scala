package chapter6

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def unlock: Machine =
    if (candies > 0) copy(locked = false, coins = coins + 1)
    else this

  def dispenseCandy: Machine =
    if (!locked && candies > 0) copy(locked = true, candies = candies - 1)
    else this
}

object Machine {

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- State.traverse(inputs)(input)
      m <- State.get
    } yield (m.coins, m.candies)

  def input(in: Input): State[Machine, Unit] =
    State.modify(m =>
      in match {
        case Coin => m.unlock
        case Turn => m.dispenseCandy
    })

}
