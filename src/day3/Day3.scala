package day3

import helpers.Helpers

object Day3 {
  def main(args: Array[String]): Unit = {
//    val input = Helpers.readFile("src/day3/test.txt").mkString("")
    val input = Helpers.readFile("src/day3/day3.txt").mkString("")
    val part1Answer = part1(input)
    println(s"Part 1: $part1Answer")

  }

  def part1(input: String): Long = {
    val multRegex = """mul\((\d{1,3}),(\d{1,3})\)""".r

    val matches = multRegex.findAllMatchIn(input).toSeq
    matches.toSeq.map(m => {
      m.group(1).toLong * m.group(2).toLong
    }).sum
  }
}
