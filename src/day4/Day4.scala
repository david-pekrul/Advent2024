package day4

import helpers.Helpers

object Day4 {
  def main(args: Array[String]): Unit = {
//    val input = Helpers.readFile("src/day4/test3.txt").toSeq.map(_.toCharArray.toSeq)
    val input = Helpers.readFile("src/day4/day4.txt").toSeq.map(_.toCharArray.toSeq)

    val part1 = solvePart1(input)
    println(s"Part 1: $part1")

  }

  def solvePart1(input: Seq[Seq[Char]]): Long = {
    val gridSearcher = new GridSearcher(new Grid(input))
    gridSearcher.countGrid()
  }
}

class Grid(input: Seq[Seq[Char]]) {
  lazy val width = input.head.size
  lazy val height = input.size

  def getCharAtCoordinate(x: Int, y: Int): Option[Char] = {
    input.lift(y).flatMap(a => a.lift(x))
  }
}

class GridSearcher(grid: Grid) {

  val FIND_WORD: Seq[(Char, Int)] = "XMAS".toCharArray.toSeq.zipWithIndex

  def countGrid(): Long = {
    (0 until grid.height).foldLeft(0L)((outerAcc, y) => {
      outerAcc + (0 until grid.width).foldLeft(0L)((innerAcc, x) => {
        innerAcc
          + horizontal(x, y)
          + horizontal(x, y, -1)
          + vertical(x, y)
          + vertical(x, y, -1)
          + diagonal1(x,y)
          + diagonal1(x,y, -1)
          + diagonal2(x,y)
          + diagonal2(x,y, -1)
      })
    })
  }

  private def horizontal(x: Int, y: Int, stepDirection: Int = 1): Long = {
    if FIND_WORD.forall(findCharAndIndex => {
      grid.getCharAtCoordinate(x + (stepDirection * findCharAndIndex._2), y).contains(findCharAndIndex._1)
    }) then
      1
    else
      0
  }

  private def vertical(x: Int, y: Int, stepDirection: Int = 1): Long = {
    if FIND_WORD.forall(findCharAndIndex => {
      grid.getCharAtCoordinate(x, y + (stepDirection * findCharAndIndex._2)).contains(findCharAndIndex._1)
    }) then
      1
    else
      0
  }

  private def diagonal1(x: Int, y: Int, stepDirection: Int = 1): Long = {
    if FIND_WORD.forall(findCharAndIndex => {
      grid.getCharAtCoordinate(x + (stepDirection * findCharAndIndex._2), y + (stepDirection * findCharAndIndex._2)).contains(findCharAndIndex._1)
    }) then
      1
    else
      0
  }

  private def diagonal2(x: Int, y: Int, stepDirection: Int = 1): Long = {
    if FIND_WORD.forall(findCharAndIndex => {
      grid.getCharAtCoordinate(x + (stepDirection * findCharAndIndex._2), y - (stepDirection * findCharAndIndex._2)).contains(findCharAndIndex._1)
    }) then
      1
    else
      0
  }


}