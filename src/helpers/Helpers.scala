package helpers

import scala.io.Source
import Vector.*

object Helpers {
  def readFile(filePath: String) = {
    Source.fromFile(filePath).getLines().toSeq
  }

  def getLetterIndex(input: Char) = {
    val intValue = input.asInstanceOf[Int]
    if (input.isLower) {
      intValue - 96
    } else {
      intValue - 38
    }
  }

  def infiniteStream[A](seed: Seq[A]): LazyList[A] = {
    val x = seed.to(LazyList)

    def xs: LazyList[A] = x #::: infiniteStream(seed)

    xs
  }

  def infiniteIndexedStream[A](seed: Seq[A]): LazyList[(A, Int)] = {
    val indexed = seed.zipWithIndex.to(LazyList)

    def xs: LazyList[(A, Int)] = indexed #::: infiniteIndexedStream(seed)

    xs
  }

  def gcf(a: Long, b: Long): Long = {
    if(b == 0){
      a
    } else {
      gcf(b,a%b)
    }
  }

  def gcf(a: BigInt, b: BigInt): BigInt = {
    if (b == 0) {
      a
    } else {
      gcf(b, a % b)
    }
  }

  def lcm(a: Long, b: Long): Long = {
    a * (b / gcf(a,b))
  }

  def lcm(input: Seq[Long]): Long = {
    if(input.isEmpty){
      ???
    }
    if(input.size == 1){
      input.head
    } else {
      input.tail.foldLeft(input.head)((acc,next) => {
        lcm(acc,next)
      })
    }
  }

  def lcm(a: BigInt, b: BigInt): BigInt = {
    a * (b / gcf(a, b))
  }

  def lcm(input: Seq[BigInt]): BigInt = {
    if (input.isEmpty) {
      ???
    }
    if (input.size == 1) {
      input.head
    } else {
      input.tail.foldLeft(input.head)((acc, next) => {
        lcm(acc, next)
      })
    }
  }
}

case class Coord(x: Int, y: Int) {
  def neighbors(): Set[Coord] = {
    ALL_DIRECTIONS.map(_.apply(this)).toSet
  }
}

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
