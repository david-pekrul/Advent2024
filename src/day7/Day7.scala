package day7

import helpers.Helpers

object Day7 {
  def main(args: Array[String]): Unit = {
//    val input = Helpers.readFile("src/day7/test.txt")
    val input = Helpers.readFile("src/day7/day7.txt")

    val parsed = input.map(MathLineParser.parse)

    val part1 = parsed.filter(_.canBeMadeTrue()).map(_.target).sum
    println(s"Part 1: $part1")

    val part2 = parsed.filter(!_.canBeMadeTrue()).filter(_.canBeMadeTrue2()).map(_.target).sum + part1
    println(s"Part 2: $part2")

  }
}

trait Expression {
  def eval(): Long
}

case class Add(left: Expression, right: Expression) extends Expression {
  override def eval(): Long = left.eval() + right.eval()
}

case class Multiply(left: Expression, right: Expression) extends Expression {
  override def eval(): Long = left.eval() * right.eval()
}

case class Concat(left: Expression, right: Expression) extends Expression {
  override def eval(): Long = (left.eval().toString + right.eval().toString).toLong
}

case class Number(n: Long) extends Expression {
  override def eval(): Long = n
}

class MathLine(val target: Long, val numbersInOrder: Seq[Long]) {

  def canBeMadeTrue(): Boolean = {
    val firstNumber = numbersInOrder.head
    val remainingNumbers = numbersInOrder.tail

    if(firstNumber == target) then return true

    val allOptions = numbersInOrder.tail.foldLeft(Set[Expression](Number(firstNumber)))((acc,next) => {
      acc.flatMap(left => {
        Set(Add(left, Number(next)), Multiply(left, Number(next)))
      })
    })

    allOptions.exists(candidate => candidate.eval() == target)
  }

  def canBeMadeTrue2(): Boolean = {
    val firstNumber = numbersInOrder.head
    val remainingNumbers = numbersInOrder.tail

    if (firstNumber == target) then return true

    val allOptions = numbersInOrder.tail.foldLeft(Set[Expression](Number(firstNumber)))((acc, next) => {
      acc.flatMap(left => {
        Set(Add(left, Number(next)), Multiply(left, Number(next)), Concat(left, Number(next)))
      })
    })

    allOptions.exists(candidate => candidate.eval() == target)
  }

}

object MathLineParser {
  def parse(input: String): MathLine = {
    val target = input.split(":")(0).toLong
    val numbersInOrder = input.split(":")(1).split(" ").filter(!_.isBlank).map(_.trim.toLong).toSeq
    MathLine(target,numbersInOrder)
  }
}
