package day5

import helpers.Helpers

object Day5 {
  def main(args: Array[String]): Unit = {
//    val input = Helpers.readFile("src/day5/test.txt").map(_.trim)
    val input = Helpers.readFile("src/day5/day5.txt").map(_.trim)

    val rules = input.takeWhile(_.nonEmpty).map(new PageRule(_))
    val orders = input.drop(rules.size+1).map(new PrintOrder(_))

    val inOrderAlready = orders.filter(order => order.isInOrder(rules))
    val part1 = inOrderAlready.map(_.middleValue()).sum

    println(s"Part 1: $part1 ")

    val outOfOrder = orders.filter(!_.isInOrder(rules))
    val sortedOrders = outOfOrder.map(a => (a,a.sort(rules)))

    val part2 = sortedOrders.map(x => PrintOrder.middleValue(x._2)).sum
    println(s"Part 2: $part2")

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

object PrintOrder {
  def middleValue(pagesAndIndexes: Map[Int, Int]): Int = {
    val middle = pagesAndIndexes.find(_._2 == (pagesAndIndexes.size / 2)).get._1
    middle
  }
}

class PrintOrder(raw: String) {
  val pagesAndIndexes: Map[Int, Int] = raw.split(",").map(_.toInt).toSeq.zipWithIndex.toMap

  def isInOrder(rules: Seq[PageRule], currentMap: Map[Int,Int] = pagesAndIndexes): Boolean = {
    rules.forall(rule => {
      val leftOrder = currentMap.get(rule.left)
      val rightOrder = currentMap.get(rule.right)
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
    PrintOrder.middleValue(pagesAndIndexes)
  }

  def sort(allRules: Seq[PageRule]): Map[Int,Int] = {
    val filteredRules = findRulesThatApply(allRules)


    def sortRecursive(remainingRules: Seq[PageRule], currentOrdering: Map[Int,Int]): Map[Int,Int] = {
        if(remainingRules.isEmpty) {
          return currentOrdering
        }
        val nextRule = remainingRules.head
        if(isInOrder(Seq(nextRule),currentOrdering)) {
          sortRecursive(remainingRules.tail, currentOrdering)
        } else {
          //swap and then add all the rules back
          val oldLeft = currentOrdering(nextRule.left)
          val oldRight = currentOrdering(nextRule.right)
          val updatedMap = currentOrdering.updated(nextRule.left,oldRight).updated(nextRule.right,oldLeft)
          sortRecursive(allRules,updatedMap) //start the search over again
        }
    }

    //start the search
    sortRecursive(allRules,pagesAndIndexes)
  }

  private def findRulesThatApply(rules: Seq[PageRule]): Seq[PageRule] = {
    rules.filter(rule => {
      pagesAndIndexes.contains(rule.left) && pagesAndIndexes.contains(rule.right)
    })
  }

}
