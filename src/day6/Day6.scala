package day6

import day6.Vector.{DOWN, LEFT, RIGHT, UP}
import helpers.Helpers

import scala.annotation.tailrec

object Day6 {
  def main(args: Array[String]): Unit = {
    //val input = Helpers.readFile("src/day6/test.txt")
    val input = Helpers.readFile("src/day6/day6.txt")

    val parse2 = GameBoard.parse(input)
    val gameBoard2 = GameBoard(parse2._1, parse2._4, parse2._2, parse2._3)
    val solution = gameBoard2.getGuardToExit()
    val part1 = solution._1.map(_._1).size
    println(s"Part 1: $part1")

    val part2 = solution._2.size
    println(s"Part 2: $part2")
  }
}

object GameBoard {
  val guardChars = Map(('^', UP), ('>', RIGHT), ('v', DOWN), ('<', LEFT))

  def parse(input: Seq[String]): (Map[Coord, Char], Int, Int, Guard) = {
    val coords: Map[Coord, Char] = input.zipWithIndex.flatMap { case (line, y) => {
      line.toCharArray.zipWithIndex.map { case (charAtCoord, x) => {
        (Coord(x, y) -> charAtCoord)
      }
      }
    }
    }.toMap

    val width = input.head.length
    val height = input.length

    val guard = coords.find(c => guardChars.contains(c._2)).map(x => Guard(Some(x._1), guardChars(x._2))).get
    (coords, width, height, guard)
  }
}


case class Coord(x: Int, y: Int) {}

case class Guard(coord: Option[Coord], vector: Vector) {

  def getCoordInFront(): Coord = {
    vector.apply(coord.get)
  }

  def rotate(): Guard = {
    Guard(coord, vector.rotate())
  }

}

object Vector extends Enumeration {
  val UP = Vector(0, -1)
  val RIGHT = Vector(1, 0)
  val DOWN = Vector(0, 1)
  val LEFT = Vector(-1, 0)
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

class GameBoard(val allPoints: Map[Coord, Char], val startingGuard: Guard, val width: Int, val height: Int) {
  val OBSTACLE = '#'
  val VISITED = 'X'
  val UNVISITED = '.'
  val obstacles = allPoints.filter(_._2 == OBSTACLE).keys.toSet
  val openPoints = allPoints.filter(_._2 != OBSTACLE).keys.toSet

  @tailrec
  private def doesGuardLoop(currentGuard: Guard, pathTraveledSoFar: Set[(Coord, Vector)], addedObstacle: Coord): Boolean = {
    val coordInFrontOfGuard = currentGuard.getCoordInFront()

    if addedObstacle == startingGuard.coord.get then return false //can't place it at the starting guard's location

    if pathTraveledSoFar.contains((currentGuard.coord.get, currentGuard.vector)) then
      //printPath(pathTraveledSoFar = pathTraveledSoFar, guard = currentGuard, addPad = true, addedObstacle)
      return true
    else if obstacles.contains(coordInFrontOfGuard) || coordInFrontOfGuard == addedObstacle then
      return doesGuardLoop(currentGuard.rotate(), pathTraveledSoFar, addedObstacle)
    else if openPoints.contains(coordInFrontOfGuard) then
      //add current point to path traveled
      val pointToAdd = (currentGuard.coord.get, currentGuard.vector)
      val updatedPath = (pathTraveledSoFar + pointToAdd)
      //move guard forward
      val updatedGuard = Guard(Some(coordInFrontOfGuard), currentGuard.vector)
      //recursively call next step
      return doesGuardLoop(updatedGuard, updatedPath, addedObstacle)
    else {
      //printPath(pathTraveledSoFar = pathTraveledSoFar, guard = currentGuard, addPad = true, addedObstacle)
      //guard is off the map. Done.
      return false
    }
  }

  def getGuardToExit(): (Set[(Coord, Vector)], Set[Coord]) = {

    @tailrec
    def _runToGuardExit(currentGuard: Guard, pathTraveledSoFar: Set[(Coord, Vector)], addedObstacles: Set[Coord]): (Set[(Coord, Vector)], Set[Coord]) = {

      //printPath(pathTraveledSoFar, currentGuard)
      val coordInFrontOfGuard = currentGuard.getCoordInFront()

      val updatedObstacles = if (
        allPoints.contains(coordInFrontOfGuard)
          && !pathTraveledSoFar.map(_._1).contains(coordInFrontOfGuard)
          && !obstacles.contains(coordInFrontOfGuard)
          && doesGuardLoop(currentGuard.rotate(), pathTraveledSoFar, coordInFrontOfGuard)) then {
        addedObstacles + coordInFrontOfGuard
      } else {
        addedObstacles
      }

      if obstacles.contains(coordInFrontOfGuard) then
        _runToGuardExit(currentGuard.rotate(), pathTraveledSoFar, updatedObstacles)
      else if openPoints.contains(coordInFrontOfGuard) then
        //add current point to path traveled
        val pointToAdd = (currentGuard.coord.get, currentGuard.vector)
        val updatedPath = (pathTraveledSoFar + pointToAdd)
        val updatedGuard = Guard(Some(coordInFrontOfGuard), currentGuard.vector)
        //move guard forward
        //recursively call next step
        _runToGuardExit(updatedGuard, updatedPath, updatedObstacles)
      else {
        //guard is off the map. Done
        val pointToAdd = (currentGuard.coord.get, currentGuard.vector)
        val updatedPath = (pathTraveledSoFar + pointToAdd)
        return (updatedPath, updatedObstacles)
      }
    }

    _runToGuardExit(startingGuard, Set(), Set())
  }

  def printPath(pathTraveledSoFar: Set[(Coord, Vector)], guard: Guard, addPad: Boolean = false, addedObstacle: Coord = Coord(-1, -1)): Unit = {

    val pathPointToVector = pathTraveledSoFar.map(x => x._1 -> x._2).toMap
    println(s"Guard Direction: ${guard.vector.toChar}")
    (0 until height).foreach(y => {
      if addPad then print("\t\t")
      (0 until width).foreach(x => {
        val currentCoord = Coord(x, y)
        if (guard.coord.contains(currentCoord)) then {
          print('G')
        } else if (addedObstacle == currentCoord) then {
          print('A')
        } else if (pathPointToVector.contains(currentCoord)) then {
          print(pathPointToVector(currentCoord).toChar)
        } else if (obstacles.contains(currentCoord)) then {
          print('#')
        } else {
          print('.')
        }
      })
      println("\r")
    })
    println("\r\n\r\n")
  }

}