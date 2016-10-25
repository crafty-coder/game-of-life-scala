package org.craftycoder

import org.scalatest.{Matchers, WordSpecLike}

class GameOfLife extends WordSpecLike with Matchers {

  type Cell = Boolean
  type Position = (Int, Int)

  def isAliveNextGen(numberOfNeighboursAlive: Int, cell: Cell): Boolean = (numberOfNeighboursAlive, cell) match {
    case (2, true) => true
    case (3, _) => true
    case _ => false
  }

  def areNeighbours(position1: Position, position2: Position): Boolean =
    position1._1 != position2._1 || position1._2 != position2._2

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

  "A position" should {
    "not be neighbour of itself" in {
      areNeighbours((0, 0), (0, 0)) shouldBe false
    }
  }

  "A position" should {
    "be neighbour of nearby cellWithPosition" in {
      areNeighbours((1, 1), (0, 0)) shouldBe true
      areNeighbours((1, 1), (0, 1)) shouldBe true
      areNeighbours((1, 1), (0, 2)) shouldBe true

      areNeighbours((1, 1), (1, 0)) shouldBe true
      areNeighbours((1, 1), (1, 2)) shouldBe true

      areNeighbours((1, 1), (2, 0)) shouldBe true
      areNeighbours((1, 1), (2, 1)) shouldBe true
      areNeighbours((1, 1), (2, 2)) shouldBe true

      areNeighbours((1, 0), (0, 0)) shouldBe true
      areNeighbours((1, 0), (0, 1)) shouldBe true
      areNeighbours((1, 0), (1, 1)) shouldBe true
      areNeighbours((1, 0), (2, 0)) shouldBe true
      areNeighbours((1, 0), (2, 1)) shouldBe true
    }
  }


}
