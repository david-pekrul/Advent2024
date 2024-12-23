package day18

import helpers.{Coord, Helpers}

import scala.annotation.tailrec

object Day18 {
  def main(args: Array[String]): Unit = {
    //val input = Helpers.readFile("src/day18/test.txt")
    val input = Helpers.readFile("src/day18/day18.txt")
    val corruptedCoords = input.map(line => {
      val split = line.split(",").toSeq
      Coord(split(0).toInt, split(1).toInt)
    })

    val DIMENSION = 70
    val grid = ByteGrid(DIMENSION, DIMENSION)
    val part1 = grid.findMinPath(corruptedCoords.take(1024))
    println(s"Part 1: $part1")

    val part2 = findBlockingByte(grid, corruptedCoords)
    println(s"Part 2: $part2")
    
  }

  def findBlockingByte(grid: ByteGrid, corrupted: Seq[Coord]): Coord = {

    def _isBlocked(start: Int, end: Int): Coord = {
      if (start == end) {
        return corrupted(start)
      }
      
      if(start + 1 == end) then {
        val finalCheck = grid.findMinPath(corrupted.take(start))
        if (finalCheck == Int.MaxValue) then
          return corrupted(start-1)
        else 
          return corrupted(end-1)
      }

      val midPoint = (start + end) / 2
      
      val isLowerBlocked = grid.findMinPath(corrupted.take(midPoint))
      
      if(isLowerBlocked == Int.MaxValue) then {
        _isBlocked(start,midPoint)
      } else {
        _isBlocked(midPoint,end)
      }
    }

    _isBlocked(0, corrupted.length)
  }
}

class ByteGrid(width: Int, height: Int) {

  def findMinPath(corrupted: Seq[Coord]): Int = {
    val end = Coord(width, height)

    val validPoints = (0 to height).flatMap(y => {
      (0 to width).map(x => {
        Coord(x, y)
      })
    }).toSet.diff(corrupted.toSet)

    @tailrec
    def _dijkstra(costs: Map[Coord, Int], visited: Set[Coord]): Map[Coord, Int] = {

      if costs(end) < Int.MaxValue then {
        return costs //?
      }

      val nextPossibles = costs
        .filter(c => !visited.contains(c._1))

      if (nextPossibles.isEmpty) {
        return costs
      }

      val (nextCoord, nextCost) = nextPossibles.minBy(_._2)
      
      if nextCost == Int.MaxValue then {
        return costs //blocked
      }
      
      val neighborCosts = nextCoord.neighbors().filter(costs.contains).map(n => (n -> (nextCost + 1)))

      val updatedCosts = neighborCosts.foldLeft(costs)((currentCosts, n) => {
        currentCosts.updatedWith(n._1)(existing => {
          Some(Math.min(existing.getOrElse(n._2), n._2))
        })
      })

      _dijkstra(updatedCosts, visited + nextCoord)
    }

    val initialCosts = validPoints.map(c => (c -> Int.MaxValue)).toMap.updated(Coord(0, 0), 0)

    val finalCosts = _dijkstra(initialCosts, Set.empty)

    finalCosts(end)
  }

}