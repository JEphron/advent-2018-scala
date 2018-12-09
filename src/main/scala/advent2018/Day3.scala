package advent2018

import util.Utils

object Day3 {
  // https://adventofcode.com/2018/day/3

  def part1(input: String): Unit = {
    // How many square inches of fabric are within two or more claims?
    val rects: List[Rect] = makeRects(input)
    val grid: Array[Array[CellState]] = makeGrid(rects)
    val i = fightForCells(rects, grid)
    println("total contested cells: ", i)
  }

  def part2(input: String): Unit = {
    // What is the ID of the only claim that doesn't overlap?
    val rects: List[Rect] = makeRects(input)
    val grid: Array[Array[CellState]] = makeGrid(rects)
    fightForCells(rects, grid)

    // search for the one & only uncontested rectangle:
    val uncontestedRect = rects.find(rect =>
      rect.points.map(point2cell(grid))
        .forall({
          case Claimed(`rect`) => true
          case _ => false
        })
    ).get
    println("uncontested rect: ", uncontestedRect.id)
  }

  private def fightForCells(rects: List[Rect], grid: Array[Array[CellState]]): Int = {
    var i = 0
    for (rect <- rects) {
      for ((ix, iy) <- rect.points) {
        grid(ix)(iy) = grid(ix)(iy) match {
          case Unclaimed => Claimed(rect)
          case Claimed(_) => i += 1; Contested
          case Contested => Contested
        }
      }
    }
    i
  }

  private def point2cell(grid: Array[Array[Day3.CellState]])(point: (Int, Int)): CellState = {
    point match {
      case (x, y) => grid(x)(y)
    }
  }

  private def makeGrid(rects: List[Rect]) = {
    val width = rects.map(it => it.x + it.w).max
    val height = rects.map(it => it.y + it.h).max
    Array.fill[CellState](width + 1, height + 1)(Unclaimed)
  }

  private def makeRects(input: String) = {
    val regex = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r
    input.lines.filterNot(_.trim.isEmpty)
      .map({
        case regex(id, x, y, w, h) =>
          Rect(id.toInt, x.toInt, y.toInt, w.toInt, h.toInt)
      }).toList
  }

  case class Rect(id: Int, x: Int, y: Int, w: Int, h: Int) {
    def points: IndexedSeq[(Int, Int)] = for {
      ix <- x until (x + w)
      iy <- y until (y + h)
    } yield (ix, iy)
  }

  sealed trait CellState

  case object Unclaimed extends CellState

  case class Claimed(rect: Rect) extends CellState

  case object Contested extends CellState

  def main(args: Array[String]): Unit = {
    val input = Utils.getInput(2018, 3)
    part1(input)
    part2(input)
  }
}
