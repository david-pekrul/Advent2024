package day1

import helpers.Helpers

object Day1 {
  def main(args: Array[String]): Unit = {
//    val input = Helpers.readFile("src/day1/test.txt")
    val input = Helpers.readFile("src/day1/day1.txt")

    val parseRegex = """(\d+)\s+(\d+)""".r
    val pairs = input.toSeq.map(x => {
      val parseRegex(num1, num2) = x

      (Integer.parseInt(num1), Integer.parseInt(num2))
    })

    val list1 = pairs.map(_._1)
    val list2 = pairs.map(_._2)

    val part1 = list1.sorted.zip(list2.sorted).map{ case (a,b) => Math.abs(a-b) }.sum
    println(s"Part 1: $part1")

  }
}
