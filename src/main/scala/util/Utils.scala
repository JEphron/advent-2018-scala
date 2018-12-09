package util
import scalaj.http.Http

import scala.io.Source

object Utils {
  val session: String = Source.fromResource("session.txt").getLines().mkString("\n")

  def getInput(year: Int, day: Int): String = {
    val url = s"https://adventofcode.com/$year/day/$day/input"
    Http(url).cookie("session", session).asString.body
  }

  def count[T](seq : Seq[T]) : Map[T, Int] = countBy[T, T](identity)(seq)

  def countBy[T, U](fn: T => U)(seq: Seq[T]): Map[U, Int] = {
    seq.foldLeft(Map[U, Int]().withDefaultValue(0))((map, x) => {
      val f = fn(x)
      map.updated(f, map(f) + 1)
    })
  }

  def benchmark(name: String, input: String, fn: String => Unit): Unit = {
    val n = 10
    val sum = List.fill(n)({
      val startTime = System.currentTimeMillis
      fn(input)
      val endTime = System.currentTimeMillis
      endTime - startTime
    }).sum
    println(s"benchmark [$name]: time was ${sum / n}ms")
  }
}
