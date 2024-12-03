package day2

import helpers.Helpers

object Day2 {
  def main(args: Array[String]): Unit = {
//    val input = Helpers.readFile("src/day2/test.txt")
    val input = Helpers.readFile("src/day2/day2.txt")
    val reports = input.map(line => line.split("\\s").map(Integer.parseInt).toSeq)

    val part1Answer = part1(input)
    println(s"Part 1: $part1Answer")

    val part2Answer = part2(input)
    println(s"Part 2: $part2Answer")

  }

  val allowedChanges = Set(1, 2, 3)

  def part1(input: Seq[String]): Int = {
    val allLines = input.map(line => line.split("\\s").map(Integer.parseInt).toSeq)
    val safeLines = allLines.filter(inSafeRange)
    safeLines.size
  }

  def part2(input: Seq[String]): Int = {
    val allLines = input.map(line => line.split("\\s").map(Integer.parseInt).toSeq)
    val safeLines1 = allLines.filter(inSafeRange)
    val safeLines2 = allLines.filter(!inSafeRange(_)).filter(x => inSafeRange2(removeValues(x)))
    safeLines1.size + safeLines2.size
  }

  def inSafeRange(report: Seq[Int]): Boolean = {
    val pairs = report.sliding(2).toSeq.map(x => (x.head, x.last))
    val diffs = pairs.map { case (a, b) => a - b }

    //if there is a change in direction, then the positive * negative will be <= 0
    val direction = sameDirection(diffs)
    val allowedRanges = diffs.forall(x => allowedChanges.contains(Math.abs(x)))
    allowedRanges && direction
  }

  def inSafeRange2(truncatedReports: Seq[Seq[Int]]): Boolean = {
    truncatedReports.exists(inSafeRange)
  }

  def removeValues(report: Seq[Int]): Seq[Seq[Int]] = {
    report.indices.map(indexToRemove => {
      report.take(indexToRemove) ++ report.drop(indexToRemove+1)
    })
  }

  def sameDirection(diffs: Seq[Int]): Boolean = {
    diffs.sliding(2).map { case List(a, b) => (a, b) }.forall{ case (a:Int,b:Int) => (a * b) > 0}
  }

  def sameDirectionCount(diffs: Seq[Int]): Int = {
    diffs.sliding(2).map { case List(a, b) => (a, b) }.count{ case (a: Int, b: Int) => (a * b) <= 0 }
  }
}
