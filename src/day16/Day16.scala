package day16

import helpers.{Coord, Helpers, Vector}

import scala.annotation.tailrec

object Day16 {
  def main(args: Array[String]): Unit = {
    //val input = Helpers.readFile("src/day16/test1.txt")
    //val input = Helpers.readFile("src/day16/test2.txt")
    val input = Helpers.readFile("src/day16/day16.txt")

    val maze = Maze.parse(input)

    val filteredMaze = maze.filterDeadEnds()

    val paths = filteredMaze.findPaths()

    val part1 = paths._1(filteredMaze.end)
    println(s"Part 1: $part1")
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

    Maze(points, start, end)
  }
}

class Maze(val points: Map[Coord, Char], val start: Coord, val end: Coord) {

  val OPEN_SYMBOLS: Set[Char] = Set('.', 'E')

  def findPaths(): (Map[Coord, Int], Map[Coord, Vector]) = {

    def _dijkstra(minCostToGetTo: Map[Coord, Int], vectorForMinCostAtCoord: Map[Coord, Vector], visited: Set[Coord]): (Map[Coord, Int], Map[Coord, Vector]) = {

      val (currentPoint, currentCost) = minCostToGetTo.filter(kv => !visited.contains(kv._1))
        .foldLeft((Coord(-1, -1), Int.MaxValue))((acc, next) => {
          if (next._2 < acc._2) {
            next
          } else {
            acc
          }
        })
      
      if(currentPoint == Coord(-1,-1)) then {
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

    val startMinCosts = filterToOnlyOpen().points.keys.map(c => c -> Int.MaxValue).toMap.updated(start, 0)
    val startVectors = Map(start -> Vector.RIGHT)

    _dijkstra(startMinCosts, startVectors, Set())
  }

  def filterDeadEnds(): Maze = {

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

}


