package day5

import helpers.Helpers

object Day5 {
  def main(args: Array[String]): Unit = {
//    val input = Helpers.readFile("src/day5/test.txt").map(_.trim)
    val input = Helpers.readFile("src/day5/day5.txt").map(_.trim)

    val rules = input.takeWhile(_.nonEmpty).map(new PageRule(_)).toSet
    val orders = input.drop(rules.size+1).map(new PrintOrder(_))

    val inOrderAlready = orders.filter(order => order.isInOrder(rules))
    val part1 = inOrderAlready.map(_.middleValue()).sum

    println(s"Part 1: $part1 ")

  }

}

object PageRule {
  val regex = """(\d+)\|(\d+)""".r
}
class PageRule(raw: String) {
  val PageRule.regex(leftString,rightString) = raw
  val left = leftString.toInt
  val right = rightString.toInt
}

class PrintOrder(raw: String) {
  val pagesAndIndexes: Map[Int, Int] = raw.split(",").map(_.toInt).toSeq.zipWithIndex.toMap

  def isInOrder(rules: Set[PageRule]): Boolean = {
    rules.forall(rule => {
      val leftOrder = pagesAndIndexes.get(rule.left)
      val rightOrder = pagesAndIndexes.get(rule.right)
      (leftOrder, rightOrder) match {
        case (None,_) => true
        case (_,None) => true
        case (Some(l:Int),Some(r:Int)) => {
          val result = l < r
          result
        }
      }
    })
  }

  def middleValue(): Int = {
    val middle = pagesAndIndexes.find(_._2 == (pagesAndIndexes.size/2)).get._1
    middle
  }
}
