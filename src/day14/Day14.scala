package day14

import helpers.Helpers
import Vector.*

object Day14 {
  def main(args: Array[String]): Unit = {
    //val input = Helpers.readFile("src/day14/test.txt")
    //val input = Helpers.readFile("src/day14/single.txt")
    val input = Helpers.readFile("src/day14/day14.txt")
    
    val robots = parse(input)
    val WIDTH = 101
    val HEIGHT = 103
    val SECONDS = 100
    val finalCoords = robots.map(r => r.move(SECONDS, WIDTH, HEIGHT))

    val part1 = score(finalCoords, width = WIDTH, height = HEIGHT)
    println(s"Part 1: $part1")
    //too low 90012120
    //        219150360
  }


  val robotRegex = """p=(.+),(.+)\sv=(.+),(.+)""".r

  def parse(input: Seq[String]): Seq[SecurityRobot] = {
    input.map(line => {
      val robotRegex(cx, cy, vx, vy) = line
      SecurityRobot(Coord(cx.toInt, cy.toInt), Vector(vx.toInt, vy.toInt))
    })
  }

  def score(coords: Seq[Coord], width: Int, height: Int): Long = {

    val halfX = width / 2
    val halfY = height / 2

    val quadrantScores = coords.foldLeft((0 to 3).map(_ -> 0L).toMap)((acc, next) => {
      if next.x < halfX then {
        if next.y < halfY then {
          acc.updatedWith(0)(_.map(_ + 1L))
        } else if (next.y > halfY) then {
          acc.updatedWith(1)(_.map(_ + 1L))
        } else {
          acc
        }
      } else if (next.x > halfX) then {
        if next.y < halfY then {
          acc.updatedWith(2)(_.map(_ + 1L))
        } else if (next.y > halfY) then {
          acc.updatedWith(3)(_.map(_ + 1L))
        } else {
          acc
        }
      } else {
        acc
      }
    })

    def mult(a: Long, b: Long) = a * b
    
    quadrantScores.values.reduceLeft(mult)
    
  }
}


class SecurityRobot(val c: Coord, val v: Vector) {
  def move(seconds: Int, width: Int, height: Int): Coord = {
    val rawCoord = v.scale(seconds).apply(c)

    val finalCoord = Coord(makePositive(rawCoord.x % width, width), makePositive(rawCoord.y % height, height))
    finalCoord
  }

  def makePositive(x: Int, mod: Int): Int = {
    if (x >= 0) then x
    else x + mod
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
