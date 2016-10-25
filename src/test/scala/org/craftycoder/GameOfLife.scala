package org.craftycoder

import org.scalatest.{Matchers, WordSpecLike}

class GameOfLife extends WordSpecLike with Matchers {

  type Cell = Boolean
  type CellWithPosition = (Cell,Int,Int)

  def isAliveNextGen(numberOfNeighboursAlive: Int, cell: Cell): Boolean = (numberOfNeighboursAlive, cell) match {
    case (2, true) => true
    case (3, _) => true
    case _ => false
  }

  def areNeighbours(cellWithPosition1: CellWithPosition, cellWithPosition2: CellWithPosition): Boolean =
    cellWithPosition1._2 != cellWithPosition2._2 ||  cellWithPosition1._3 != cellWithPosition2._3

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

  "A cellWithPosition" should {
    "not be neighbour of itself" in {
      areNeighbours((true, 0, 0), (true, 0, 0)) shouldBe false
    }
  }

  "A cellWithPosition" should {
    "be neighbour of nearby cellWithPosition" in {
      areNeighbours((true,1, 1), (true,0, 0)) shouldBe true
      areNeighbours((true,1, 1), (true,0, 1)) shouldBe true
      areNeighbours((true,1, 1), (true,0, 2)) shouldBe true

      areNeighbours((true,1, 1), (true,1, 0)) shouldBe true
      areNeighbours((true,1, 1), (true,1, 2)) shouldBe true

      areNeighbours((true,1, 1), (true,2, 0)) shouldBe true
      areNeighbours((true,1, 1), (true,2, 1)) shouldBe true
      areNeighbours((true,1, 1), (true,2, 2)) shouldBe true

      areNeighbours((true,1, 0), (true,0, 0)) shouldBe true
      areNeighbours((true,1, 0), (true,0, 1)) shouldBe true
      areNeighbours((true,1, 0), (true,1, 1)) shouldBe true
      areNeighbours((true,1, 0), (true,2, 0)) shouldBe true
      areNeighbours((true,1, 0), (true,2, 1)) shouldBe true
    }
  }



}
