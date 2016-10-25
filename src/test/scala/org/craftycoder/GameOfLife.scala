package org.craftycoder

import org.scalatest.{Matchers, WordSpecLike}

class GameOfLife extends WordSpecLike with Matchers {

  type Cell = Boolean

  def isAliveNextGen(numberOfNeighboursAlive: Int, cell: Cell): Boolean = (numberOfNeighboursAlive, cell) match {
    case (2, true) => true
    case (3, _) => true
    case _ => false
  }

  "Any cell" should {
    "not be alive on next generation if it has less than 2 neighbours alive" in {
      isAliveNextGen(numberOfNeighboursAlive = 0, cell = true) shouldBe false
      isAliveNextGen(numberOfNeighboursAlive = 1, cell = true) shouldBe false

      isAliveNextGen(numberOfNeighboursAlive = 0, cell = false) shouldBe false
      isAliveNextGen(numberOfNeighboursAlive = 1, cell = false) shouldBe false
    }
  }

  "A cell" should {
    "be alive on next generation if it has 2 or 3 neighbours alive" in {
      isAliveNextGen(numberOfNeighboursAlive = 2, cell = true) shouldBe true
      isAliveNextGen(numberOfNeighboursAlive = 3, cell = true) shouldBe true
    }
  }

  "A dead cell" should {
    "not be alive on next generation if it has 2 neighbours alive" in {
      isAliveNextGen(numberOfNeighboursAlive = 2, cell = false) shouldBe false
    }
  }

  "Any cell" should {
    "not be alive on next generation if it has more than 3 neighbours alive" in {
      isAliveNextGen(numberOfNeighboursAlive = 4, cell = false) shouldBe false
      isAliveNextGen(numberOfNeighboursAlive = 4, cell = true) shouldBe false
    }
  }

  "A dead cell" should {
    "be alive on next generation if it has 3 neighbours alive" in {
      isAliveNextGen(numberOfNeighboursAlive = 3, cell = false) shouldBe true
    }
  }



}
