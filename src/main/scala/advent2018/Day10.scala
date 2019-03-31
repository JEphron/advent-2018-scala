package advent2018

import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import util.Utils


object Day10 {
  def main(args: Array[String]): Unit = {
    val input = Utils.getInput(2018, 10)
    val points = parseInput(input)

    val data = Iterator.iterate(points)(advance).sliding(2).dropWhile {
      case Seq(a, b) => a.map(_.y).max > b.map(_.y).max
    }.take(1).next().head

    saveImage(data)
  }

  def advance(points: List[Point]): List[Point] = {
    points.map(point => point.copy(x = point.x + point.vx, y = point.y + point.vy))
  }

  def parseInput(input: String) = {
    val pattern = "position=<\\s*(-?\\d+),\\s*(-?\\d+)> velocity=<\\s*(-?\\d+),\\s*(-?\\d+)>".r
    input.lines.map { case pattern(x, y, vx, vy) => Point(x.toInt, y.toInt, vx.toInt, vy.toInt) }.toList
  }

  def saveImage(points: List[Point]): Unit = {
    val imWidth = 1500
    val imHeight = 500

    val image = new BufferedImage(imWidth, imHeight, BufferedImage.TYPE_INT_RGB)

    val xmax = points.map(_.x).max
    val xmin = points.map(_.x).min
    val ymax = points.map(_.y).max
    val ymin = points.map(_.y).min

    def remap(value: Float, low1: Float, high1: Float, low2: Float, high2: Float): Float = {
      low2 + (value - low1) * (high2 - low2) / (high1 - low1)
    }

    val graphics = image.createGraphics()
    for (Point(x, y, _, _) <- points) {
      val rx = remap(x, xmin, xmax, 50, imWidth - 50)
      val ry = remap(y, ymin, ymax, 50, imHeight - 50)
      graphics.drawRect(rx.toInt, ry.toInt, 3, 3)
    }

    val file = new File("/advent-day-10.png")
    ImageIO.write(image, "png", file)
    println("wrote", file)
  }

  case class Point(x: Int, y: Int, vx: Int, vy: Int)

}
