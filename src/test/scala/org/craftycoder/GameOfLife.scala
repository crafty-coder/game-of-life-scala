package org.craftycoder

import org.scalatest.{Matchers, WordSpecLike}

class GameOfLife extends WordSpecLike with Matchers {

  def isAliveNextGen(numberOfNeighboursAlive: Int): Boolean = false

  "A cell" should {
    "not be alive on next generation if it has less than 2 neighbours alive" in {
      isAliveNextGen(numberOfNeighboursAlive = 0) shouldBe false
      isAliveNextGen(numberOfNeighboursAlive = 1) shouldBe false
    }
  }

}
