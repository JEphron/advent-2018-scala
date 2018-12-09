package advent2018

import util.Utils

import scala.collection.mutable

object Day6 {
  // https://adventofcode.com/2018/day/6

  def part1(input: String): Unit = {
    // What is the size of the largest area that isn't infinite?

    val inputPoints = parseInput(input)
    val width = inputPoints.maxBy(_.x).x
    val height = inputPoints.maxBy(_.y).y
    val grid = Array.ofDim[String](width, height)
    val ignored = mutable.Set[String]("")

    for (gridPoint@Point(ix, iy) <- gridIterator(width, height)) {
      val pointsByDistance = inputPoints.map(point => (point, distance(point, gridPoint))).sortBy(_._2)
      pointsByDistance match {
        case (closestPoint, closestDistance) :: (_, secondClosestDistance) :: _ =>
          if (closestDistance != secondClosestDistance) {
            grid(ix)(iy) = closestPoint.toString
          }
          val isOnBorder = ix == 0 || ix == width - 1 || iy == 0 || iy == height - 1
          if (isOnBorder) {
            ignored.add(grid(ix)(iy))
          }
      }
    }


    val (a, b) = grid.flatten.filterNot(ignored).groupBy(it => it).maxBy(_._2.length)
    println(a, b.length)
  }

  def part2(input: String): Unit = {
    val inputPoints = parseInput(input)
    val width = inputPoints.maxBy(_.x).x
    val height = inputPoints.maxBy(_.y).y

    val answer = gridIterator(width, height).map(gridPoint =>
      inputPoints.map(distance(_, gridPoint)).sum
    ).count(_ < 10000)

    println(answer)
  }

  private def parseInput(input: String): List[Point] = {
    val pairPattern = "(\\d+), (\\d+)".r
    input.lines.map { case pairPattern(x, y) => Point(x.toInt, y.toInt) }.toList
  }

  def distance(point1: Point, point2: Point): Int =
    Math.abs(point1.x - point2.x) + Math.abs(point1.y - point2.y)

  def gridIterator(width: Int, height: Int): Seq[Point] =
    for {
      x <- 0 until width
      y <- 0 until height
    } yield Point(x, y)

  case class Point(x: Int, y: Int)

  def main(args: Array[String]): Unit = {
    val input = Utils.getInput(2018, 6)
    part1(input)
  }
}
