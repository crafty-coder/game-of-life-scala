package org.craftycoder

object Demo extends GameOfLife with App {

  def generateRandomSeed(size: Int, cellAliveChance: Double): Board = (for (x <- 0.until(size); y <- 0.until(size)) yield (math.random < cellAliveChance, (x, y))).toList

  def printBoard(board: Board): Unit = {

    println("---------------")

    board.foreach({
      case (true, (_, 0)) => print("\nX")
      case (false, (_, 0)) => print("\n ")
      case (true, (_, _)) => print("X")
      case (false, (_, _)) => print(" ")
    })

    println("")
    println("---------------")

  }

  val seed = generateRandomSeed(size = 10, cellAliveChance = 0.3)

  gameOfLife(seed) take 50 foreach { board => printBoard(board) }

}
