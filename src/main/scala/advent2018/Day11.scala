package advent2018

object Day11 {

  def main(args: Array[String]): Unit = {
    def part1(): Unit = {
      val serialNumber = 4455
      val grid = Array.ofDim[Int](300, 300)
      val fuelCells = map2d[Int, Int](grid, (x, y, _) => powerLevelForFuelCell(x, y, serialNumber))
      val sums = patchwiseSums(fuelCells, 100, 100)
      println(sums.maxBy(_._3))
    }

    def part2(): Unit = {
      val serialNumber = 4455
      val grid = Array.ofDim[Int](300, 300)
      val fuelCells = map2d[Int, Int](grid, (x, y, _) => powerLevelForFuelCell(x, y, serialNumber))

      val sumsOfAllSizes = for (dimension <- 1 until 300) yield {
        println(dimension)
        (dimension, patchwiseSums(fuelCells, dimension, dimension).maxBy(_._3))
      }

      println(sumsOfAllSizes.maxBy(_._2._3))
    }

    part1()
    part2()
  }

  def map2d[T: Manifest, Q: Manifest](grid: Array[Array[T]], fn: (Int, Int, T) => Q): Array[Array[Q]] = {
    grid.zipWithIndex.map { case (n, x) => n.zipWithIndex.map { case (m, y) => fn(x, y, m) } }
  }

  def patchwiseSums(grid: Array[Array[Int]], width: Int, height: Int): Seq[(Int, Int, Int)] = {
    // todo: probably a faster DP way to do this
    (for {
      i <- grid.indices
      j <- grid(i).indices
    } yield {
      if (i + width > grid.length || j + height > grid(i).length) {
        None
      } else {
        var sum = 0
        for {
          dx <- 0 until width
          dy <- 0 until height
        } {
          val ix = (i + dx) min grid.length
          val iy = (j + dy) min grid(i).length
          sum += grid(ix)(iy)
        }
        Some((i, j, sum))
      }
    }).collect { case Some(x) => x }
  }

  def powerLevelForFuelCell(x: Int, y: Int, serialNumber: Int): Int = {
    val rackId = x + 10
    (((rackId * y + serialNumber) * rackId / 100) % 10) - 5
  }

  case class Point(x: Int, y: Int)

}
