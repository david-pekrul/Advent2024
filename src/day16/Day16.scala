package day16

import helpers.Vector.*
import helpers.{Coord, Helpers, Vector}

import scala.annotation.tailrec

object Day16 {
  def main(args: Array[String]): Unit = {
    //val input = Helpers.readFile("src/day16/test1.txt")
    //val input = Helpers.readFile("src/day16/test2.txt")
    val input = Helpers.readFile("src/day16/day16.txt")

    val maze = Maze.parse(input)
    
    val (part1, part2) = maze.part2()
    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
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
    /* THIS IS DEAD CODE. It works, but I solved it "better" in part2" */
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

  def part2(): (Int,Int) = {

    @tailrec
    def _pathBuilder(paths: Set[Seq[(MazePoint,Int)]], minCosts: Map[MazePoint,Int] = Map()): Set[Seq[(MazePoint,Int)]] = {

      if(paths.forall(p => p.last._1.c == end)){
        return paths
      }
      
      val updatedPaths = paths.flatMap(path => {
        if(path.last._1.c == end) then {
          Seq(path)
        } else {

          val currentLast = path.last

          val stepForward = (currentLast._1.getForward(), currentLast._2 + 1)
          val rotateRight = (currentLast._1.getRotateRight(), currentLast._2 + 1000)
          val rotateLeft = (currentLast._1.getRotateLeft(), currentLast._2 + 1000)

          def isValidRotation(r: MazePoint): Boolean = {
            !points.get(r.getForward().c).contains('#')
          }

          val updates = (if (points.contains(stepForward._1.c) && !(points(stepForward._1.c) == '#')) then Set(path :+ stepForward) else Set.empty)
            ++ (if (isValidRotation(rotateLeft._1)) then Set(path :+ rotateLeft) else Set.empty)
            ++ (if (isValidRotation(rotateRight._1)) then Set(path :+ rotateRight) else Set.empty)
          updates
        }
      })
      
      val updatedMinCosts = updatedPaths.foldLeft(minCosts)((currentMinCosts,nextPath) => {
        currentMinCosts.updatedWith(nextPath.last._1)(existingOpt => Some(Math.min(existingOpt.getOrElse(Int.MaxValue),nextPath.last._2)))
      })
      
      //remove the paths that got to a different point, but with higher cost
      val validUpdatedPaths = updatedPaths.filter(p => {
        updatedMinCosts(p.last._1) == p.last._2
      })
      
      _pathBuilder(validUpdatedPaths, updatedMinCosts)
    }

    val paths = _pathBuilder(Set(Seq((MazePoint(start,RIGHT),0))))
    
    val minPathCost = paths.map(_.last._2).min //this is part 1!
    val minPaths = paths.toSeq.filter(_.last._2 == minPathCost)
    val pointsOnMinPaths = minPaths.flatMap(path => path.map(_._1.c)).toSet
    (minPathCost, pointsOnMinPaths.size)
  }
}

case class MazePoint(val c: Coord, val v: Vector) {
  def getForward(): MazePoint = {
    MazePoint(v.apply(c), v)
  }

  def getRotateLeft(): MazePoint = {
    MazePoint(c, v.rotate().reverse())
  }

  def getRotateRight(): MazePoint = {
    MazePoint(c, v.rotate())
  }

  def getBackwards(): MazePoint = {
    MazePoint(c, v.reverse())
  }

  def getNeighbors(): Seq[MazePoint] = {
    Seq(getForward(), getRotateLeft(), getRotateRight())
  }
}


