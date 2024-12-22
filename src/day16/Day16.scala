package day16

import helpers.Vector.*
import helpers.{Coord, Helpers, Vector}

import scala.annotation.tailrec

object Day16 {
  def main(args: Array[String]): Unit = {
    val input = Helpers.readFile("src/day16/test1.txt")
    //val input = Helpers.readFile("src/day16/test2.txt")
    //val input = Helpers.readFile("src/day16/day16.txt")

    val maze = Maze.parse(input)


    val paths = maze.findPaths()

//    val part1 = paths._1(maze.end)
//    println(s"Part 1: $part1")

    val part2data = maze.findPaths2()
    val part1_2 = part2data._1.filter(_._1.c == maze.end).values.min

    println(s"part1_2: $part1_2")
    
    val part2 = Maze.walkBack(part2data._1, part2data._2, maze.end, maze.start)
    println(s"Part 2: $part2")
    //499 too high
    //417 too low
    //https://www.youtube.com/watch?v=ro2SSxd21JM

  }
}

object Maze {
  def parse(input: Seq[String]): Maze = {
    val points = input.zipWithIndex.flatMap { case (line, y) => {
      line.toCharArray.zipWithIndex.map { case (char, x) => {
        (Coord(x, y) -> char)
      }
      }
    }
    }.toMap

    val start = points.find(_._2 == 'S').get._1
    val end = points.find(_._2 == 'E').get._1

    Maze(points, start, end).filterDeadEnds().filterToOnlyOpen()
  }
  
  def walkBack(costs: Map[MazePoint,Int], previouses: Map[MazePoint, Set[MazePoint]], end: Coord, start: Coord): Int = {
    val endPoints = costs.filter(_._1.c == end)
    val endMinimum = endPoints.values.min
    val minEndPoints = endPoints.filter(_._2 == endMinimum) //there might be multiple directions to get to the end with the min cost
    
    @tailrec
    def _back(nexts: Seq[MazePoint], visited: Set[Coord]): Set[Coord] = {
      if(nexts.isEmpty) then {
        return visited
      }
      val updated = nexts.flatMap(n => previouses.get(n)).flatten
      _back(updated, visited ++ updated.map(_.c))
    }
    
    val allVisited = _back(minEndPoints.keys.toSeq, minEndPoints.map(_._1.c).toSet)
    allVisited.size
  }
}

class Maze(val points: Map[Coord, Char], val start: Coord, val end: Coord) {

  val OPEN_SYMBOLS: Set[Char] = Set('.', 'E')

  def findPaths(): (Map[Coord, Int], Map[Coord, Vector]) = {

    @tailrec
    def _dijkstra(minCostToGetTo: Map[Coord, Int], vectorForMinCostAtCoord: Map[Coord, Vector], visited: Set[Coord]): (Map[Coord, Int], Map[Coord, Vector]) = {

      val (currentPoint, currentCost) = minCostToGetTo.filter(kv => !visited.contains(kv._1))
        .foldLeft((Coord(-1, -1), Int.MaxValue))((acc, next) => {
          if (next._2 < acc._2) {
            next
          } else {
            acc
          }
        })

      if (currentPoint == Coord(-1, -1)) then {
        return (minCostToGetTo, vectorForMinCostAtCoord)
      }

      val currentVector = vectorForMinCostAtCoord(currentPoint)
      val neighborsAndDirections = Vector.ALL_DIRECTIONS
        .map(v => (v.apply(currentPoint) -> v))
        .filter(nv => minCostToGetTo.contains(nv._1))
        .toMap
      val neighborsDirectionsCost = neighborsAndDirections.toSeq.map { case (n, v) => {
        if (v == currentVector) {
          (n, v, currentCost + 1)
        } else {
          (n, v, currentCost + 1000 + 1)
        }
      }
      }

      val (updatedCostToGetTo, updatedVectorForMinCostAtCoord) = neighborsDirectionsCost.foldLeft((minCostToGetTo, vectorForMinCostAtCoord))((acc, next) => {
        val (currentCost, currentVectors) = acc
        if (currentCost(next._1) > next._3) then {
          //the new cost is lower
          (acc._1.updated(next._1, next._3), acc._2.updated(next._1, next._2))
        } else {
          acc
        }
      })


      _dijkstra(updatedCostToGetTo, updatedVectorForMinCostAtCoord, visited + currentPoint)
    }

    val startMinCosts = points.keys.map(c => c -> Int.MaxValue).toMap.updated(start, 0)
    val startVectors = Map(start -> Vector.RIGHT)

    _dijkstra(startMinCosts, startVectors, Set())
  }

  def filterDeadEnds(): Maze = {

    @tailrec
    def _filter(current: Map[Coord, Char]): Map[Coord, Char] = {
      val deadEndPoints = current
        .filter(_._2 == '.')
        .filter(p => {
          val neighbors = p._1.neighbors()
          val wallCount = neighbors.toSeq.map(n => current.get(n)).count(n => n.contains('#'))
          wallCount == 3
        })
        .map(p => (p._1 -> '#'))

      if (deadEndPoints.isEmpty) {
        return current
      }

      _filter(current ++ deadEndPoints)
    }

    val filteredPoints = _filter(points)

    Maze(filteredPoints, start, end)
  }

  def filterToOnlyOpen(): Maze = {
    Maze(points.filter(_._2 != '#'), start, end)
  }

  def findPaths2(): (Map[MazePoint, Int], Map[MazePoint, Set[MazePoint]]) = {

    val mazePointStart = MazePoint(start, RIGHT)
    val startMinCosts = points.flatMap(p => {
        ALL_DIRECTIONS.map(v => MazePoint(p._1, v))
      }).map(mp => (mp -> Int.MaxValue)).toMap
      .updated(mazePointStart, 0)

    val endPoints = startMinCosts.filter(_._1.c == end)
    
    @tailrec
    def _dijkstra2(
                    minCostToGetTo: Map[MazePoint, Int], //For a point state, the min cost to get there
                    pointToPrevious: Map[MazePoint, Set[MazePoint]], //for a point state, what are the states that got to it for its min cost
                    visited: Set[MazePoint] //points that face into a wall or backwards
                  ): (Map[MazePoint, Int], Map[MazePoint, Set[MazePoint]]) = {

      val possibleNextPoints = minCostToGetTo
        .filter(_._2 != Int.MaxValue)
        .filter(mpc => {
          !visited.contains(mpc._1)
        })

      if (possibleNextPoints.isEmpty) then {
        return (minCostToGetTo, pointToPrevious)
      }

      val (currentPoint, currentCost) = possibleNextPoints.minBy(_._2)
      val minEndCost = endPoints.map(ep => minCostToGetTo(ep._1)).min

      def neighborsDirectionsCost = currentPoint
        .getNeighbors().filter(n => {
          points.contains(n.c)
        }).map { n => {
          if (n == currentPoint.getForward()) {
            (n, currentCost + 1)
          } else {
            (n, currentCost + 1000)
          }
        }}
        .filter(n => n._2 <= minEndCost)

      val (updatedCostToGetTo, updatedVectorForMinCostAtCoord) = neighborsDirectionsCost.foldLeft((minCostToGetTo, pointToPrevious))((acc, next) => {
        val (latestCost, latestPrevious) = acc
        if (latestCost(next._1) > next._2) then {
          //the new cost is lower
          val updatedCostMap = acc._1.updated(next._1, next._2)
          val updatedPreviousMap = latestPrevious.updatedWith(next._1)(v => Some(v.getOrElse(Set()) + currentPoint))
          (updatedCostMap, updatedPreviousMap)
        } else {
          acc
        }
      })
      
      _dijkstra2(updatedCostToGetTo, updatedVectorForMinCostAtCoord, visited + currentPoint)
    }

    _dijkstra2(startMinCosts, Map(), Set())
  }
}

case class MazePoint(val c: Coord, val v: Vector) {
  def getForward(): MazePoint = {
    MazePoint(v.apply(c), v)
  }

  def getRotations(): Seq[MazePoint] = {
    Seq(MazePoint(c, v.rotate()), MazePoint(c, v.rotate().reverse()))
  }

  def getBackwards(): MazePoint = {
    MazePoint(c, v.reverse())
  }

  def getNeighbors(): Seq[MazePoint] = {
    getRotations() :+ getForward()
  }
}


