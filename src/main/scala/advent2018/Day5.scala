package advent2018

import util.Utils

object Day5 {
  // https://adventofcode.com/2018/day/5

  def part1(string: String): Unit = {
    // How many units remain after fully reacting the polymer you scanned?
    println(react(string).length)
  }

  def part2(string: String): Unit = {
    // What is the length of the shortest polymer you can produce
    // by removing all units of exactly one type and fully reacting the result?
    println("abcd".map(ch => react(string.filterNot(_.toLower == ch)))
      .minBy(_.length).length)
  }

  private def react(string: String): String = {
    // [..] adjacent units of the same type and opposite polarity are destroyed [..]
    // In abBA, bB destroys itself, leaving aA [..] this then destroys itself, leaving nothing.
    string.trim().foldLeft(List[Char]())({
      case (Nil, char) => List(char)
      case (head :: xs, char) =>
        if ((head.isLower ^ char.isLower) && (head.toLower == char.toLower)) {
          xs
        } else {
          char :: head :: xs
        }
    }).reverse.mkString("")
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.getInput(2018, 5)
    part1(input)
    part2(input)
  }
}
