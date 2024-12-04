package day3

import helpers.Helpers

import scala.util.matching.Regex

object Day3 {
  def main(args: Array[String]): Unit = {
//    val input = Helpers.readFile("src/day3/test.txt").mkString("")
//    val input = Helpers.readFile("src/day3/test2.txt").mkString("")
    val input = Helpers.readFile("src/day3/day3.txt").mkString("")
    val part1 = part1Func(input)
    println(s"Part 1: $part1")

    val part2 = part2Func(input)
    println(s"Part 2: $part2")

  }

  def part1Func(input: String): Long = {
    val multRegex = """mul\((\d{1,3}),(\d{1,3})\)""".r

    val matches = multRegex.findAllMatchIn(input).toSeq
    matches.toSeq.map(m => {
      m.group(1).toLong * m.group(2).toLong
    }).sum
  }

  def part2Func(input: String): Long = {
    val instructionRegex = """mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)""".r

    val matches = instructionRegex.findAllMatchIn(input).toSeq

    val activeOps = matches.foldLeft((true,Seq[Regex.Match]())){ case((use,ops),next) => {
      input.substring(next.start,next.end) match {
        case "do()" => (true,ops)
        case "don't()" => (false,ops)
        case _ => {
          if use then
            (use, ops :+ next)
          else
            ((use, ops))
        }
      }
    }}._2

    activeOps.map(m => {
      m.group(1).toLong * m.group(2).toLong
    }).sum
  }
}
