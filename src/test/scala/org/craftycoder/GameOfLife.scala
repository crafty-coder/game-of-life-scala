package org.craftycoder

import org.scalatest.{Matchers, WordSpecLike}

class GameOfLife extends WordSpecLike with Matchers {

  def isAliveNextGen(numberOfNeighboursAlive: Int, isAlive: Boolean): Boolean = (numberOfNeighboursAlive, isAlive) match {
    case (2, true) => true
    case (3, _) => true
    case _ => false
  }

  "A cell" should {
    "not be alive on next generation if it has less than 2 neighbours alive" in {
      isAliveNextGen(numberOfNeighboursAlive = 0, isAlive = true) shouldBe false
      isAliveNextGen(numberOfNeighboursAlive = 1, isAlive = true) shouldBe false
    }
  }

  "A cell" should {
    "be alive on next generation if it has 2 or 3 neighbours alive" in {
      isAliveNextGen(numberOfNeighboursAlive = 2, isAlive = true) shouldBe true
      isAliveNextGen(numberOfNeighboursAlive = 3, isAlive = true) shouldBe true
    }
  }

  "A dead cell" should {
    "not be alive on next generation if it has 2 neighbours alive" in {
      isAliveNextGen(numberOfNeighboursAlive = 2, isAlive = false) shouldBe false
    }
  }

  "Any cell" should {
    "not be alive on next generation if it has more than 3 neighbours alive" in {
      isAliveNextGen(numberOfNeighboursAlive = 4, isAlive = false) shouldBe false
      isAliveNextGen(numberOfNeighboursAlive = 4, isAlive = true) shouldBe false
    }
  }

  "A dead cell" should {
    "be alive on next generation if it has 3 neighbours alive" in {
      isAliveNextGen(numberOfNeighboursAlive = 3, isAlive = false) shouldBe true
    }
  }


}
