package advent2018

import util.Utils

import scala.collection.mutable

object Day7 {
  // https://adventofcode.com/2018/day/7

  def part1(input: String): Unit = {
    val graph = parseInput(input)
    val result = mutable.ListBuffer[Char]()
    val roots = graph.keys.filter(key => graph.values.forall(!_.contains(key))).toList
    val q = mutable.PriorityQueue[Char](roots: _*)(Ordering.Char.reverse)
    val visited = mutable.Set[Char]()

    def prerequisites(n: Char) = graph.filter({ case (_, values) => values.contains(n) }).keys

    while (q.nonEmpty) {
      val node = q.dequeue()
      visited.add(node)
      result.append(node)
      val toAdd = graph(node)
        .filterNot(visited) // not visited
        .filterNot(it => q.exists(_ == it)) // not in the queue
        .filter(c => prerequisites(c).forall(visited.contains)) // all prerequisites satisfied
      q.enqueue(toAdd: _*)
    }
    println(result.mkString)
  }

  def parseInput(input: String): Map[Char, List[Char]] = {
    val pattern = "Step (.) must be finished before step (.) can begin.".r
    val acc = Map[Char, List[Char]]().withDefaultValue(Nil)
    input.lines.foldLeft(acc)({
      case (map, pattern(fst, snd)) => map.updated(fst.head, snd.head :: map(fst.head))
    })
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.getInput(2018, 7)
    part1(input)
  }
}
