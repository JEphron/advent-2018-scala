package advent2018

import java.util

import scala.collection.mutable.ArrayBuffer

object Day9 {
  def main(args: Array[String]): Unit = {
    //    println(solve(10, 200))
    //    println(solve(10, 1618))
    //    println(solve(13, 7999))
    //    println(solve(17, 1104))
    //    println(solve(21, 6111))
    //    println(solve(30, 5807))
    println(solve(464, 70918))

  }

  def solve(numPlayers: Int, lastMarble: Int): Int = {
    // todo: looks like the Josephus problem, probably has either a DP or closed form solution
    val playerScores = new Array[Int](numPlayers)
    val marbles = ArrayBuffer[Int](0)
    var currentMarbleIndex = 0
    for (marble <- 1 until lastMarble) {
      if (marble % 23 == 0) {
        var marbleToRemove = currentMarbleIndex - 7
        if (marbleToRemove < 0) {
          marbleToRemove = marbles.size + marbleToRemove
        }
        val popped = marbles.remove(marbleToRemove)
        val currentPlayer = (marble - 1) % numPlayers
        playerScores(currentPlayer) += marble + popped
        currentMarbleIndex = marbleToRemove
      } else {
        currentMarbleIndex = currentMarbleIndex + 2
        if (currentMarbleIndex > marbles.size) {
          currentMarbleIndex -= marbles.size
        }
        marbles.insert(currentMarbleIndex, marble)
      }
      //      val marbleString = marbles.zipWithIndex.map { case (m, i) =>
      //        if (i == currentMarbleIndex) s"•" else "-"
      //      }.mkString("")
      //      println(s"[] $marbleString")
    }

    playerScores.max
  }
}
