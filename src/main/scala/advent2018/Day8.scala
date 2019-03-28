package advent2018

import util.Utils

import scala.collection.mutable

object Day8 {

  case class Node(children: mutable.MutableList[Node], metadata: Array[Int])

  def main(args: Array[String]): Unit = {
    val input = Utils.getInput(2018, 8)
    val ints = input.split(' ').map(_.trim.toInt)
    println(metadataSum(parseTree(ints)))
    println(valueOfNode(parseTree(ints)))
  }

  def metadataSum(node: Node): Int =
    node.metadata.sum + node.children.map(metadataSum).sum


  def valueOfNode(node: Node): Int = {
    val subOne: Int => Int = (x: Int) => x - 1
    if (node.children.isEmpty) {
      node.metadata.sum
    } else {
      node.metadata.map(subOne
        andThen node.children.get
        andThen (_.map(valueOfNode))
        andThen (_.getOrElse(0))
      ).sum
    }
  }

  def parseTree(input: Array[Int]): Node =
    parseTreeHelper(input, 0)._1

  def parseTreeHelper(input: Array[Int], startIndex: Int, depth: Int = 0): (Node, Int) = {
    val numChildNodes = input(startIndex)
    val numMetadataNodes = input(startIndex + 1)

    var childStartIndex = startIndex + 2
    val node = Node(mutable.MutableList.empty, new Array[Int](numMetadataNodes))
    for (_ <- 0 until numChildNodes) {
      val (childNode, childEnd) = parseTreeHelper(input, childStartIndex, depth + 1)
      childStartIndex = childEnd
      node.children += childNode
    }

    Array.copy(input, childStartIndex, node.metadata, 0, numMetadataNodes)
    childStartIndex += numMetadataNodes
    (node, childStartIndex)
  }
}
