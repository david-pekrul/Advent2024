package day6

import day6.Vector.{DOWN, LEFT, RIGHT, UP}
import helpers.Helpers

import scala.annotation.tailrec

object Day6 {
  def main(args: Array[String]): Unit = {
//    val input = Helpers.readFile("src/day6/test.txt")
    val input = Helpers.readFile("src/day6/day6.txt")

    val gameBoard = GameBoard.parse(input)
    val part1Solved = gameBoard.solve1()
//    part1Solved.printBoard()

    val part1 = part1Solved.countVisited()
    println(s"Part 1: $part1")
  }

}

object GameBoard {
  val guardChars = Map(('^',UP),('>',RIGHT),('v',DOWN),('<',LEFT))

  def parse(input: Seq[String]): GameBoard = {
    val coords = input.zipWithIndex.map {case (line, y) => {
      line.toCharArray.zipWithIndex.map { case (charAtCoord, x) => {
        (Coord(x,y),charAtCoord)
      }
      }
    }}

    val width = coords(0).size
    val height = coords.size

    val guard = coords.flatten.find(c => guardChars.contains(c._2)).map(x => Guard(Some(x._1), guardChars(x._2))).get
    GameBoard(coords.flatten.toMap,guard, width, height)
  }
}

class GameBoard(val startingPoints: Map[Coord,Char], val startingGuard: Guard, val width: Int, val height: Int) {
  val OBSTACLE = '#'
  val VISITED = 'X'
  val UNVISITED = '.'


  def solve1(): GameBoard = {

    @tailrec
    def _runToGuardExit(currentGameBoard: GameBoard): GameBoard = {

//      currentGameBoard.printBoard()

      if (currentGameBoard.startingGuard.coord.isEmpty) {
        return currentGameBoard
      }

      val coordInFrontOfGuard = currentGameBoard.startingPoints.get(currentGameBoard.startingGuard.getCoordInFront())
      coordInFrontOfGuard match {
        case None => {
          //end condition - The guard is facing the edge of the map
          //set map's current position to VISITED
          val updatedMap = currentGameBoard.startingPoints.updated(currentGameBoard.startingGuard.coord.get, VISITED)
          //move guard off map
          val updatedGuard = Guard(None, currentGameBoard.startingGuard.vector)
          return makeNext(updatedMap, updatedGuard)
        }
        case Some(UNVISITED) | Some(VISITED) => {
          val updatedMap = currentGameBoard.startingPoints.updated(currentGameBoard.startingGuard.coord.get, VISITED)
          val updatedGuard = Guard(Some(currentGameBoard.startingGuard.getCoordInFront()), currentGameBoard.startingGuard.vector)
          return _runToGuardExit(makeNext(updatedMap, updatedGuard))
        }
        case Some(OBSTACLE) => {
          val next = makeNext(currentGameBoard.startingPoints, currentGameBoard.startingGuard.rotate())
          return _runToGuardExit(next)
        }
      }
    }

    _runToGuardExit(this)

  }

  def makeNext(nextPoints: Map[Coord,Char], nextGuard: Guard): GameBoard = {
    GameBoard(nextPoints, nextGuard, width, height)
  }

  def countVisited(): Long = {
    startingPoints.count(_._2==VISITED)
  }

  def printBoard(): Unit = {
    (0 until height).foreach(y => {
      (0 until width).foreach(x => {
        val currentCoord = Coord(x,y)
        if(startingGuard.coord.contains(currentCoord)) {
          val guardChar = GameBoard.guardChars.find(x => x._2 == startingGuard.vector).get._1.toString
          print(s"$guardChar")
        } else {
          print(startingPoints(currentCoord))
        }
      })
      println("\r")
    })
    println("\r\n\r\n")
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

  def rotate(): Vector = {
    this match {
      case UP => RIGHT
      case RIGHT => DOWN
      case DOWN => LEFT
      case LEFT => UP
    }
  }
}