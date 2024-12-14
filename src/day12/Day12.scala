package day12

import day12.Vector.{ALL_DIRECTIONS, DOWN, LEFT, RIGHT, UP}
import helpers.Helpers

import scala.annotation.tailrec

object Day12 {
  def main(args: Array[String]): Unit = {
    //val input = Helpers.readFile("src/day12/test1.txt")
    //val input = Helpers.readFile("src/day12/test2.txt")
    val input = Helpers.readFile("src/day12/day12.txt")

    val rawGarden = Garden.parse(input)
    val compactedGarden = rawGarden.combineRegions()

    val part1 = compactedGarden.score()
    println(s"Part 1: $part1")
    
    val part2 = compactedGarden.score2()
    println(s"Part 2: $part2")
  }
}

object Garden {
  def parse(input: Seq[String]): Garden = {
    val coordToChar = input.zipWithIndex.flatMap { case (line, y) => {
      line.toCharArray.zipWithIndex.map { case (id, x) => {
        Coord(x, y) -> id
      }}
    }}.toMap

    val regions = coordToChar.keys.map(coord => GardenRegion(Set(coord))).toSet

    Garden(coordToChar, regions)
  }
}

class Garden(val plotToChar: Map[Coord,Char], val regions: Set[GardenRegion]) {

  def combineRegions(): Garden = {

    @tailrec
    def _compact(remaingingSets: Set[GardenRegion], compacted: Set[GardenRegion]): Set[GardenRegion] = {
      if remaingingSets.isEmpty then {
        return compacted
      }

      val firstSet = remaingingSets.head
      val overlappingOther = remaingingSets.tail.find(otherSet => otherSet.canCombine(firstSet))

      if overlappingOther.isEmpty then {
        return _compact(remaingingSets.tail, compacted+firstSet)
      }

      val updated = GardenRegion(firstSet.plots ++ overlappingOther.get.plots)

      return _compact(remaingingSets.removedAll(Seq(overlappingOther.get, firstSet)) + updated, compacted)
    }

    val idToAllRegions = regions.map(r => plotToChar(r.plots.head) -> r).groupMap(_._1)(_._2)
    val result = idToAllRegions.values.flatMap(r => _compact(r,Set.empty)).toSet
    Garden(plotToChar,result)
  }

  def score(): Long = {
    regions.toSeq.map(r => {
      val id = plotToChar(r.plots.head)
      val idScore = r.score()
      //println(s"$id => $idScore")
      idScore
    }).sum
  }

  def score2(): Long = {
    regions.toSeq.map(r => {
      val id = plotToChar(r.plots.head)
      val idScore = r.score2()
      //println(s"$id => $idScore")
      idScore
    }).sum
  }
}

class GardenRegion(val plots: Set[Coord]) {
  def canCombine(other: GardenRegion): Boolean = {
    this.plots.exists(plot => {
      ALL_DIRECTIONS.map(_.apply(plot)).exists(other.plots.contains)
    })
  }

  def score(): Long = {
    val fences = plots.toSeq.flatMap(plot => ALL_DIRECTIONS.map(_.apply(plot))).filter(x => !plots.contains(x))
    val area = plots.size

    return area.longValue * fences.size.longValue
  }

  def score2(): Long = {
    @tailrec
    def _combineFences(remaining: Set[Set[(Coord,Vector)]], grouped: Set[Set[(Coord,Vector)]]): Set[Set[(Coord,Vector)]] = {
      if remaining.isEmpty then {
        return grouped
      }

      val first = remaining.head
      val overlappingOpt = remaining.tail
        .filter(other => other.head._2 == first.head._2)
        .find(other => {
          other.flatMap(o => ALL_DIRECTIONS.map(v => v.apply(o._1))).exists(first.toMap.keySet.contains)
        })
      
      if overlappingOpt.isEmpty then {
        return _combineFences(remaining.tail,grouped+first.toSet)
      }
      
      val combined = overlappingOpt.get ++ first
      return _combineFences(remaining.tail.removedAll(Seq(overlappingOpt.get))+combined,grouped)
    }

    val area = plots.size.longValue
    val fencesAndVectors = plots
      .flatMap(plot => ALL_DIRECTIONS.map(v => (v.apply(plot), v)).filter(x => !plots.contains(x._1)))
      .map(cv => Set(cv))

    val fenceCount = _combineFences(fencesAndVectors, Set.empty)
    
    val cost = fenceCount.size.longValue * area
    cost
  }
}

case class Coord(x: Int, y: Int) {}

object Vector extends Enumeration {
  val UP: Vector = Vector(0, -1)
  val RIGHT: Vector = Vector(1, 0)
  val DOWN: Vector = Vector(0, 1)
  val LEFT: Vector = Vector(-1, 0)

  val ALL_DIRECTIONS: Seq[Vector] = Seq(UP, DOWN, LEFT, RIGHT)
}

case class Vector(deltaX: Int, deltaY: Int) {
  def apply(coord: Coord): Coord = {
    Coord(coord.x + deltaX, coord.y + deltaY)
  }

  def scale(s: Int): Vector = {
    Vector(deltaX * s, deltaY * s)
  }

  def rotate(): Vector = {
    this match {
      case UP => RIGHT
      case RIGHT => DOWN
      case DOWN => LEFT
      case LEFT => UP
    }
  }

  def reverse(): Vector = {
    this match {
      case UP => DOWN
      case RIGHT => LEFT
      case DOWN => UP
      case LEFT => RIGHT
    }
  }

  lazy val toChar: Char = this match {
    case UP => '^'
    case RIGHT => '>'
    case DOWN => 'v'
    case LEFT => '<'
  }
}
