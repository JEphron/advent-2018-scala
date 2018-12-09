package advent2018

import util.Utils

import scala.collection.immutable.HashSet
import scala.collection.mutable

object Day1 {
  // https://adventofcode.com/2018/day/1

  def part1(input: String): Unit = {
    // Starting with a frequency of zero,
    // what is the resulting frequency after all of the
    // changes in frequency have been applied?
    println(input.lines.map(_.toInt).sum)
  }

  // bench: 48ms
  def part2fast(input: String): Unit = {
    // What is the first frequency your device reaches twice?
    var m = mutable.Set[Int]()
    var acc = 0
    val lines = input.lines.map(_.toInt).toArray
    var i = 0
    var j = 0
    // takeaways: use array instead of list, list.length is mad slow
    while (!m.contains(acc)) {
      m += acc
      acc += lines(i)
      i = (i + 1) % lines.length
      j += 1
    }
    println(acc)
  }

  //bench: 592ms
  def part2functional(input: String): Unit = {

    // infinite lazy stream
    def xs: Stream[Int] = input.lines.map(_.toInt).toStream #::: xs

    val sums = xs.scanLeft(0)(_ + _)
    val uniques = sums.scanLeft(HashSet[Int]())(_ + _)
    uniques.zip(sums).find({
      case (set, num) => set.contains(num)
    }) match {
      case Some((_, n)) => println("n:", n)
      case None => println("nothing")
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.getInput(2018, 1)
    part1(input)
    part2fast(input)
  }
}
