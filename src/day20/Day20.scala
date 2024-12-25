package day20

import helpers.{Coord, Helpers, Vector}

import scala.annotation.tailrec

object Day20 {
  def main(args: Array[String]): Unit = {
    //val input = Helpers.readFile("src/day20/test.txt")
    val input = Helpers.readFile("src/day20/day20.txt")

    val maze = RaceMaze.parse(input)

    val cheatPoints = maze.findCheats()
//    val part1 = cheatPoints.count(_._2 >= 100)
    val part1 = cheatPoints.count(_._2 >= 100)
    println(s"Part 1: $part1")
  }
}

object RaceMaze {
  def parse(input: Seq[String]): RaceMaze = {
    val points = input.zipWithIndex.flatMap { case (line, y) => {
      line.toCharArray.zipWithIndex.map { case (char, x) => {
        (Coord(x, y) -> char)
      }
      }
    }
    }.toMap.filter(_._2 != '#')

    RaceMaze(points, input.head.length, input.length)
  }
}

class RaceMaze(val points: Map[Coord, Char], val width: Int, val height: Int) {

  lazy val start = points.find(_._2 == 'S').get._1
  lazy val end = points.find(_._2 == 'E').get._1

  lazy val noCheatDistances = calcNoCheatPointCosts()

  private def calcNoCheatPointCosts(): Map[Coord, Int] = {

    @tailrec
    def _dijkstra(currentCosts: Map[Coord, Int], visited: Set[Coord]): Map[Coord, Int] = {

      val lastVisitedOpt = currentCosts.filterNot(n => visited.contains(n._1)).minByOption(_._2)

      if (lastVisitedOpt.isEmpty) then {
        return currentCosts
      }

      val (lastVisited, currentCost) = lastVisitedOpt.get

      val updatedCosts = lastVisited.neighbors().foldLeft(currentCosts)((costs, neighbor) => {
        if (costs.contains(neighbor)) then {
          costs.updatedWith(neighbor)(existing => existing.map(x => Math.min(x, currentCost + 1)))
        } else {
          costs
        }
      })

      _dijkstra(updatedCosts, visited + lastVisited)
    }


    val startingData = points.keys.map(p => p -> Int.MaxValue).toMap.updated(start, 0)

    _dijkstra(startingData, Set.empty)
  }

  def findCheats(): Seq[(Coord,Int)] = {
    val thing = points.keys.toSeq.flatMap(startPoint => {
      val x = Vector.ALL_DIRECTIONS.map(v => v.apply(startPoint) -> v.apply(v.apply(startPoint)))
        .map(skip => (skip._1 -> (noCheatDistances.getOrElse(skip._2, 0) - noCheatDistances(startPoint) - 2)))
      x
    })
    thing.filter(_._2 > 0)
  }
}


