package chapter6

import org.scalatest.wordspec.AnyWordSpec

class MachineTest extends AnyWordSpec {
  "input on coin" when {
    val state = Machine.input(Coin)
    "locked" should {
      val machine = Machine(locked = true, 2, 2)
      val (_, m) = state(machine)

      "unlock the machine" in {
        assert(!m.locked)
      }

      "accept coin" in {
        assert(m.coins == machine.coins + 1)
      }
    }
    "unlocked" should {
      val machine = Machine(locked = false, 2, 2)
      val (_, m) = state(machine)

      "accept coin when unlocked" in {
        assert(m.coins == machine.coins + 1)
      }
    }

    "out of candies" should {
      val machine = Machine(locked = true, 0, 2)
      val (_, m) = state(machine)

      "ignores when out of candies" in {
        assert(m == machine.copy(candies = 0))
      }
    }
  }

  "input on Turn" when {
    val state = Machine.input(Turn)

    "unlocked" should {
      val machine = Machine(locked = false, 2, 2)
      val (_, m) = state(machine)

      "lock it" in {
        assert(m.locked)
      }

      "dispense candy" in {
        assert(m.candies == machine.candies - 1)
      }
    }

    "locked" should {
      val machine = Machine(locked = true, 2, 2)
      val (_, m) = state(machine)

      "keep it locked" in {
        assert(m.locked)
      }

      "don't dispense candy" in {
        assert(m.candies == machine.candies)
      }
    }

    "out of candies" should {
      val machine = Machine(locked = false, 0, 2)
      val (_, m) = state(machine)

      "don't dispense candy" in {
        assert(m.candies == machine.candies)
      }
    }
  }

  "simulateMachine" when {
    "processes multiple inputs" should {
      val machine = Machine(locked = true, 10, 5)
      val state = Machine.simulateMachine(
        List(Coin, Turn, Turn, Coin, Coin, Turn, Coin, Turn, Coin, Turn))
      val ((coins, candies), _) = state(machine)

      "properly calculate coins" in {
        assert(coins == machine.coins + 5)
      }

      "properly calculate candies" in {
        assert(candies == machine.candies - 4)
      }
    }
  }
}
