package day15

import helpers.Helpers
import Vector.*
import Factory.*

import scala.annotation.tailrec

object Day15 {
  def main(args: Array[String]): Unit = {
    //val input = Helpers.readFile("src/day15/test1.txt")
    //val input = Helpers.readFile("src/day15/test2.txt")
    val input = Helpers.readFile("src/day15/day15.txt")

    val (factory,operations) = Factory.parse(input)
    val after = factory.run(operations)
    
    val part1 = Factory.score(after.initialPoints)
    println(s"Part 1: $part1")
  }
}

object Factory {
  val WALL = '#'
  val ROBOT = '@'
  val BOX = 'O'
  val OPEN = '.'

  def parse(input: Seq[String]): (Factory,Seq[Vector]) = {
    val factoryInput = input.takeWhile(!_.trim.isBlank)
    val instructionInput = input.dropWhile(!_.trim.isBlank).filter(_.nonEmpty)
      .mkString("").toCharArray.toSeq.map(Vector.charToVector)

    val points = factoryInput.zipWithIndex.flatMap{ case (line, y) => {
      line.toCharArray.zipWithIndex.map{ case (char, x) => {
        (Coord(x,y) -> char)
      }}
    }}.toMap

    val robot = points.find(_._2 == ROBOT).get._1
    val width = factoryInput.head.length
    val height = factoryInput.length
    (Factory(points,robot,width,height), instructionInput)
  }
  
  def score(points: Map[Coord,Char]): Long = {
    points.filter(_._2 == BOX).keys.map(box => {
      box.y * 100L + box.x.longValue
    }).sum
  }
}

class Factory(val initialPoints: Map[Coord,Char], val initialRobot: Coord, val width: Int, val height: Int) {

  def run(instructions: Seq[Vector]): Factory = {
    val finalPoints = _run(instructions, initialPoints, initialRobot)
    Factory(finalPoints, finalPoints.find(_._2 == ROBOT).get._1, width, height)
  }
  
  @tailrec
  private def _run(instructions: Seq[Vector], currentPoints: Map[Coord,Char], robot: Coord): Map[Coord,Char] = {
    
    
    
    if(instructions.isEmpty) then {
      printFactory(currentPoints, width, height)
      return currentPoints
    }
    

    @tailrec
    def _canMove(startPoint: Coord, instruction: Vector): Boolean = {
      val pointAhead = instruction.apply(startPoint)
      currentPoints(instruction.apply(startPoint)) match {
        case WALL => false
        case BOX => _canMove(pointAhead,instruction)
        case OPEN => true
      }
    }
    
    def _move(startPoint: Coord, instruction: Vector, incomingChar: Char, currentMap: Map[Coord,Char]): Map[Coord,Char] = {
      val pointAhead = instruction.apply(startPoint)
      val objectAhead = currentPoints(instruction.apply(startPoint)) 
      currentPoints(instruction.apply(startPoint)) match {
        case BOX => {
          _move(pointAhead, instruction, BOX, currentMap).updated(pointAhead,incomingChar)
        }
        case OPEN => {
          currentMap.updated(pointAhead,incomingChar)
        }
      }
    }
    
    val nextInstruction = instructions.head
    if (_canMove(robot, nextInstruction)) then {
      val afterMove = _move(robot,nextInstruction,ROBOT,currentPoints).updated(robot,'.')
      _run(instructions.tail, afterMove, nextInstruction.apply(robot))
    } else {
      _run(instructions.tail, currentPoints, robot)
    }
  }
  
  def printFactory(currentPoints: Map[Coord,Char], width: Int, height: Int): Unit = {
    println("----------------------------------")
    (0 until height).foreach(y => {
      (0 until width).foreach(x => {
        print(s"${currentPoints(Coord(x,y))}")
      })
      println()
    })
  }
  println("----------------------------------\r\n")
}

case class Coord(x: Int, y: Int) {}

object Vector extends Enumeration {
  val UP: Vector = Vector(0, -1)
  val RIGHT: Vector = Vector(1, 0)
  val DOWN: Vector = Vector(0, 1)
  val LEFT: Vector = Vector(-1, 0)

  val ALL_DIRECTIONS: Seq[Vector] = Seq(UP, DOWN, LEFT, RIGHT)

  def charToVector(c: Char): Vector = {
    c match
    {
      case '^' => UP
      case '>' => RIGHT
      case 'v' => DOWN
      case '<' => LEFT
    }
  }
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
