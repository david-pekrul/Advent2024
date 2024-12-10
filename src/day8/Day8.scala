package day8

import helpers.Helpers

object Day8 {
  def main(args: Array[String]): Unit = {
//    val input = Helpers.readFile("src/day8/test.txt")
    val input = Helpers.readFile("src/day8/day8.txt")

    val radioMap = parseRadioMap(input)

    val part1 = radioMap.findAntiNodes().flatten.toSet.size
    println(s"Part 1: $part1")
    
    val part2 = radioMap.findAntiNodes((0 until Math.max(radioMap.width,radioMap.height)).toSet).flatten.toSet.size
    println(s"Part 2: $part2")
  }

  def parseRadioMap(input: Seq[String]): RadioMap = {
    val towers = input.zipWithIndex.map{ case(line,y) => {
      line.toCharArray.zipWithIndex.map{ case (char, x) => {
        (char,Coord(x,y))
      }}
    }}

    val towerGroups = towers.flatten.filter(_._1!='.').groupBy(_._1).map(x => (x._1 -> x._2.map(_._2)))
    RadioMap(towerGroups, towers.head.length, towers.length)
  }
}

case class Coord(x: Int, y: Int) {
  def onBoard(width: Int, height: Int): Boolean = {
    0 <= x && x <= (width-1) && 0 <= y  &&  y <= (height-1)
  }

  def getVector(other: Coord): Vector = {
    Vector(other.x - this.x, other.y - this.y)
  }
}

case class Vector(dX: Int, dY: Int) {
  def apply(input: Coord): Coord = {
    Coord(input.x + dX, input.y + dY)
  }

  def reverse(): Vector = {
    scale(-1)
  }

  def scale(s: Int): Vector = {
    Vector(s*dX, s*dY)
  }
}

class RadioMap(val towers: Map[Char, Seq[Coord]], val width: Int, val height: Int) {
  def findAntiNodes(scalers: Set[Int] = Set(1)): Seq[Set[Coord]] = {
    towers.values.map(t => findAntiNodesForFreq(t, scalers)).toSeq
  }

  private def findAntiNodesForFreq(towers: Seq[Coord], scalers: Set[Int]): Set[Coord] = {
    val towersWithIndicies = towers.zipWithIndex

    val antiNodes = towersWithIndicies.flatMap { case (tower1, index1) => {
      towersWithIndicies.drop(index1+1).flatMap { case (tower2, index2) => {
        val vector = tower1.getVector(tower2)
        val vectors = scalers.map(s => vector.scale(s))
        val antiNodesFromPair = vectors.flatMap(v => Set(v.reverse().apply(tower1), v.apply(tower2))).filter(_.onBoard(width, height))
        antiNodesFromPair
      }}
    }}

    antiNodes.toSet
  }
}