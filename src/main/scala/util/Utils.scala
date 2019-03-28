package util
import java.io.{File, FileReader, FileWriter}
import java.util.Scanner
import java.util.prefs.Preferences

import scalaj.http.Http

import scala.io.Source

object Utils {
  val session: String = Source.fromResource("session.txt").getLines().mkString("\n")

  def getInput(year: Int, day: Int): String = {
    val preferences = Preferences.userNodeForPackage(Utils.getClass)
    val prefsKey = s"cached-advent-input-$year-$day"
    val cacheFilePath = preferences.get(prefsKey, "")
    if (cacheFilePath == "") {
      val url = s"https://adventofcode.com/$year/day/$day/input"
      val input = Http(url).cookie("session", session).asString.body
      val file = File.createTempFile(prefsKey, "txt")
      val writer = new FileWriter(file)
      writer.write(input)
      writer.close()
      preferences.put(prefsKey, file.getAbsolutePath)
      input
    } else {
      Source.fromFile(cacheFilePath).mkString
    }
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
