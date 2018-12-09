package advent2018

import util.Utils

object Day2 {
  // https://adventofcode.com/2018/day/2

  def part1(input: String): Unit = {
    // What is the checksum for your list of box IDs?
    val countsForLines = input.lines.map(Utils.count(_)).toList
    val doubles = countsForLines.filter(_.values.exists(_ == 2))
    val triples = countsForLines.filter(_.values.exists(_ == 3))
    println(doubles.length * triples.length)
  }

  def part2(input: String): Unit = {
    // The boxes will have IDs which differ by exactly one character
    // at the same position in both strings.
    // What letters are common between the two correct box IDs?
    // todo: I'm sure this is overkill
    def levenshteinDistance(a: String, b: String): Int = {
      def min3(a: Int, b: Int, c: Int): Int = math.min(a, math.min(b, c))

      val d = Array.ofDim[Int](a.length, b.length)
      for (i <- 0 until a.length) {
        d(i)(0) = i
      }
      for (j <- 0 until b.length) {
        d(0)(j) = j
      }
      for {
        j <- 1 until b.length
        i <- 1 until a.length
      } yield {
        if (a(i - 1) == b(j - 1)) {
          d(i)(j) = d(i - 1)(j - 1)
        } else {
          d(i)(j) = min3(
            d(i - 1)(j) + 1,
            d(i)(j - 1) + 1,
            d(i - 1)(j - 1) + 1
          )
        }
      }
      d(a.length - 1)(b.length - 1)
    }

    val list = input.lines.toList
    val head = list.combinations(2).filter({
      case a :: b :: _ => levenshteinDistance(a, b) == 1
    }).map({
      case a :: b :: _ => a.filterNot(_ == a.diff(b).head)
    }).toList.head
    println(head)
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.getInput(2018, 2)
    part1(input)
    part2(input)
  }
}
