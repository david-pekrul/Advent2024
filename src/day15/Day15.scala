package day15

import helpers.Helpers
import Vector.*
import Factory.*

import scala.annotation.tailrec

object Day15 {
  def main(args: Array[String]): Unit = {
    //val input = Helpers.readFile("src/day15/test1.txt")
//    val input = Helpers.readFile("src/day15/test2.txt")
//    val input = Helpers.readFile("src/day15/test3.txt")
    val input = Helpers.readFile("src/day15/day15.txt")

    val (factory,operations) = Factory.parse(input)
    val after = factory.run(operations)
    
    val part1 = Factory.score(after.initialPoints)
    println(s"Part 1: $part1")
    
    val (factory2,operations2) = Factory.parse2(input)
    factory2.printFactory(factory2.initialPoints,factory2.width,factory2.height)
    
    val after2 = factory2.run(operations2)
    after2.printFactory(after2.initialPoints,after2.width,after2.height)
    val part2 = Factory.score2(after2.initialPoints)
    println(s"Part 2: $part2")
  }
}

object Factory {
  val WALL = '#'
  val ROBOT = '@'
  val BOX = 'O'
  val OPEN = '.'
  val BOX_LEFT = '['
  val BOX_RIGHT = ']'
  
  
  
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

  def parse2(input: Seq[String]): (Factory2, Seq[Vector]) = {
    val factoryInput = input.takeWhile(!_.trim.isBlank)
    val instructionInput = input.dropWhile(!_.trim.isBlank).filter(_.nonEmpty)
      .mkString("").toCharArray.toSeq.map(Vector.charToVector)

    val points = factoryInput.zipWithIndex.flatMap { case (line, y) => {
      line.toCharArray.zipWithIndex.flatMap { case (char, x) => {
        val doubleWideEntry = char match {
          case BOX =>   Map(Coord(2*x,y) -> BOX_LEFT, Coord(2*x+1,y) -> BOX_RIGHT)
          case OPEN =>  Map(Coord(2*x,y) -> OPEN,     Coord(2*x+1,y) -> OPEN)
          case ROBOT => Map(Coord(2*x,y) -> ROBOT,    Coord(2*x+1,y) -> OPEN)
          case WALL =>  Map(Coord(2*x,y) -> WALL,     Coord(2*x+1,y) -> WALL)
          case _ => Map()
        }
        doubleWideEntry
      }}
    }}.toMap

    val robot = points.find(_._2 == ROBOT).get._1
    val width = factoryInput.head.length*2
    val height = factoryInput.length
    (Factory2(points, robot, width, height), instructionInput)
  }
  
  def score(points: Map[Coord,Char]): Long = {
    points.filter(_._2 == BOX).keys.map(box => {
      box.y * 100L + box.x.longValue
    }).sum
  }

  def score2(points: Map[Coord, Char]): Long = {
    points.filter(_._2 == BOX_LEFT).keys.map(box => {
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

class Factory2(val initialPoints: Map[Coord, Char], val initialRobot: Coord, val width: Int, val height: Int) {

  def run(instructions: Seq[Vector]): Factory = {
    val finalPoints = _run(instructions, initialPoints, initialRobot)
    Factory(finalPoints, finalPoints.find(_._2 == ROBOT).get._1, width, height)
  }

  @tailrec
  private def _run(instructions: Seq[Vector], currentPoints: Map[Coord, Char], robot: Coord): Map[Coord, Char] = {

    if (instructions.isEmpty) then {
      //printFactory(currentPoints, width, height)
      return currentPoints
    }

    //printFactory(currentPoints, width, height)
    
    def _canMove(startPoint: Coord, instruction: Vector): Boolean = {
      val pointAhead = instruction.apply(startPoint)
      currentPoints(instruction.apply(startPoint)) match {
        case WALL => false
        case BOX_LEFT => {
          //todo: take vector direction into account and get the other side of the box
          instruction match {
            case LEFT | RIGHT => {
              _canMove(pointAhead, instruction)
            }
            case UP | DOWN => {
              val rightBox = RIGHT.apply(pointAhead)
              return _canMove(pointAhead, instruction) && _canMove(rightBox,instruction)
            }
          }
        }
        case BOX_RIGHT => {
          //todo: take vector direction into account and get the other side of the box
          instruction match {
            case LEFT | RIGHT => {
              _canMove(pointAhead, instruction)
            }
            case UP | DOWN => {
              val leftBox = LEFT.apply(pointAhead)
              return _canMove(pointAhead, instruction) && _canMove(leftBox,instruction)  
            }
          }
        }
        case OPEN => true
      }
    }
    
    
    def _getPointsToMove(startPoint: Coord, instruction: Vector, currentMap: Map[Coord, Char]): Set[Coord] = {
      val objectAtPoint = currentMap(startPoint)
      objectAtPoint match {
        case BOX_LEFT => {
          instruction match {
            case LEFT | RIGHT => {
              Set(startPoint) ++ _getPointsToMove(instruction.apply(startPoint), instruction, currentMap)    
            }
            case UP | DOWN => {
              Set(startPoint, RIGHT.apply(startPoint)) //both coords of the box
              ++ _getPointsToMove(instruction.apply(startPoint), instruction, currentMap)
              ++ _getPointsToMove(RIGHT.apply(instruction.apply(startPoint)), instruction, currentMap)
            }
          }
        }
        case BOX_RIGHT => {
          instruction match {
            case LEFT | RIGHT => {
              Set(startPoint) ++ _getPointsToMove(instruction.apply(startPoint), instruction, currentMap)
            }
            case UP | DOWN => {
              Set(startPoint, LEFT.apply(startPoint)) //both coords of the box
                ++ _getPointsToMove(instruction.apply(startPoint), instruction, currentMap)
                ++ _getPointsToMove(LEFT.apply(instruction.apply(startPoint)), instruction, currentMap)
            }
          }
        }
        case ROBOT => {
          Set(startPoint) ++ _getPointsToMove(instruction.apply(startPoint), instruction, currentMap)
        }
        case OPEN => {
          Set.empty
        }
      }
    }

    def applyMove(pointsToMove: Set[Coord], instruction: Vector, currentMap: Map[Coord, Char]): Map[Coord, Char] = {
      val movedPointMap = pointsToMove.map(c => (instruction.apply(c) -> currentMap(c))).toMap
      val pointsVacated = pointsToMove.removedAll(movedPointMap.keys).map(c => (c -> '.')).toMap
      
      val withMovedPoints = (currentMap ++ movedPointMap) ++ pointsVacated
      withMovedPoints
    }

    val nextInstruction = instructions.head
    if (_canMove(robot, nextInstruction)) then {
      val pointsToMove = _getPointsToMove(robot, nextInstruction, currentPoints)
      val afterMove = applyMove(pointsToMove, nextInstruction, currentPoints)
      _run(instructions.tail, afterMove, nextInstruction.apply(robot))
    } else {
      _run(instructions.tail, currentPoints, robot)
    }
  }

  def printFactory(currentPoints: Map[Coord, Char], width: Int, height: Int, tab: Boolean = false): Unit = {
    println("----------------------------------")
    (0 until height).foreach(y => {
      if(tab) then print("\t")
      (0 until width).foreach(x => {
        print(s"${currentPoints(Coord(x, y))}")
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
