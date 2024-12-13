package day10

import helpers.Helpers
import day10.Vector.{DOWN, LEFT, RIGHT, UP}

import scala.annotation.tailrec

object Day10 {
  def main(args: Array[String]): Unit = {
//    val input = Helpers.readFile("src/day10/test.txt")
    val input = Helpers.readFile("src/day10/day10.txt")
    
    val topoMap = TopoMap.parse(input)
    
    val fullTrails = topoMap.findTrails()
    
    val part1 = TopoMap.score(fullTrails)
    println(s"Part 1: $part1")

    val part2 = TopoMap.score2(fullTrails)
    println(s"Part 2: $part2")

  }
}

class TopoMap(adjMap: Map[Coord,Set[Coord]], coordToDigit: Map[Coord,Int]) {
  def findTrails(): Seq[Trail] = {
    val zeros = coordToDigit.filter(_._2 == 0).keys.toSeq

    val startTrails = zeros.map(z => Trail(z,z))

    @tailrec
    def _takeStepForTrails(currentTrails: Seq[Trail], nextNumber: Int, targetInt: Int): Seq[Trail] = {
      if(nextNumber > targetInt) then {
        return currentTrails
      }
      val updatedTrails = currentTrails.flatMap(trail => {
        trail.getNextNeighbors()
          .filter(n => coordToDigit.get(n).contains(nextNumber))
          .map(n => Trail(trail.start,n))
      })
      
//      updatedTrails.foreach(t => System.out.println(t.print(coordToDigit)))
      
      _takeStepForTrails(updatedTrails, nextNumber+1, targetInt)
    }
    
    _takeStepForTrails(startTrails,1,9)
  }
}

object TopoMap {
  def parse(input: Seq[String]): TopoMap = {
    val coordToDigit = input.zipWithIndex.flatMap{ case (line, y) => {
      line.toCharArray.map(_.toInt - '0'.toInt).zipWithIndex.map{ case (digit,x) => {
        Coord(x,y) -> digit
      }}
    }}.toMap

    val adjMap = coordToDigit.keys.map(coord => {
      coord -> Vector.ALL_DIRECTIONS.map(_.apply(coord)).filter(coordToDigit.contains).toSet
    }).toMap

    TopoMap(adjMap, coordToDigit)
  }
  
  def score(trails: Seq[Trail]): Int = {
    val scores = trails.groupBy(t => t.start).map(x => x._1 -> x._2.map(_.currentEnd).toSet.size)
    scores.values.sum
  }

  def score2(trails: Seq[Trail]): Int = {
    val scores = trails.groupBy(t => t.start).map(x => x._1 -> x._2.map(_.currentEnd).size)
    scores.values.sum
  }
}

class Trail(val start: Coord, val currentEnd: Coord) {
  def getNextNeighbors(): Seq[Coord] = {
    Vector.ALL_DIRECTIONS.map(d => d.apply(currentEnd))
  }

  override def toString: String = {
    s"Trail ${start}->${currentEnd}"
  }
  
  def print(coordToDigit: Map[Coord,Int]): String = {
    s"Trail ${start}[${coordToDigit(start)}]->${currentEnd}[${coordToDigit(currentEnd)}]"
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