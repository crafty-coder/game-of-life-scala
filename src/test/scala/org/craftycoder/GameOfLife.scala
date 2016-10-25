package org.craftycoder

import org.scalatest.{Matchers, WordSpecLike}

class GameOfLife extends WordSpecLike with Matchers {

  type Cell = Boolean
  type Position = (Int, Int)
  type CellBoard = (Cell, Position)
  type Board = List[CellBoard]


  def isAliveNextGen(numberOfNeighboursAlive: Int, cell: Cell): Boolean = (numberOfNeighboursAlive, cell) match {
    case (2, true) => true
    case (3, _) => true
    case _ => false
  }

  def areNeighbours(p1: Position, p2: Position): Boolean = {

    val dist = (p1._1 - p2._1, p1._2 - p2._2)

    dist match {
      case (0, 0) => false
      case (i, j) if Math.abs(i) < 2 && Math.abs(j) < 2 => true
      case _ => false
    }
  }

  def nextGen(board: Board): Board = board.map({ case (cell, position) => (false, position) })


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
    "be neighbour of nearby positions" in {
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

  "A position" should {
    "not be neighbour of positions that are not nearby" in {
      areNeighbours((1, 1), (1, 3)) shouldBe false
      areNeighbours((1, 1), (1, -1)) shouldBe false

      areNeighbours((1, 1), (-1, 1)) shouldBe false
      areNeighbours((1, 1), (3, 1)) shouldBe false

      areNeighbours((1, 1), (3, 3)) shouldBe false
    }
  }

  "A 1x1 board which contains a dead cell" should {
    "stay the same in the next gen" in {
      val initialBoard = List((false, (0, 0)))
      val expectedBoard = List((false, (0, 0)))

      nextGen(initialBoard) shouldBe expectedBoard
    }
  }

  "A 1x1 board which contains a living cell" should {
    "contain a dead cell in the next gen" in {
      val initialBoard = List((true, (0, 0)))
      val expectedBoard = List((false, (0, 0)))

      nextGen(initialBoard) shouldBe expectedBoard
    }
  }

  "A 2x2 board which contains a dead cells" should {
    "stay the same in the next gen" in {
      val initialBoard = List(
        (false, (0, 0)), (false, (0, 1)),
        (false, (1, 0)), (false, (1, 1))
      )
      val expectedBoard = List(
        (false, (0, 0)), (false, (0, 1)),
        (false, (1, 0)), (false, (1, 1))
      )

      nextGen(initialBoard) shouldBe expectedBoard
    }
  }


}
