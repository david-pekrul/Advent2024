package day21

import day21.ArrowPad.{buildPaths, data}
import helpers.Helpers
import helpers.Vector
import helpers.Coord

object Day21 {
  def main(args: Array[String]): Unit = {
    //val input = Helpers.readFile("src/day21/test.txt")
    val input = Helpers.readFile("src/day21/day21.txt")

    val part1 = input.map(code => getScore(code,2)).sum
    println(s"Part 1: $part1")

    val part2 = input.map(code => getScore(code, 25)).sum
    println(s"Part 2: $part2")
  }

  def getScore(code: String, numberOfArrowRobots: Int): Long = {
    val robots = Seq(NumPad) ++ (0 until numberOfArrowRobots).map(x => ArrowPad)
    val paths = robots.zipWithIndex.foldLeft(Set(code))((currentCodes, robot) => {
      val next = currentCodes.flatMap(x => robot._1.getShortestPaths(x))
      next
    })

    val minLength = paths.map(_.length).min
    val codeNumber = code.replace("A", "").toInt

    minLength * codeNumber
  }
}

trait Robot {

  def getData(): String

  lazy val shortestPaths = buildPaths(getData())

  def buildPaths(data: String): Map[(Char, Char), Set[Seq[Vector]]] = {
    val points = data.split("\r\n").zipWithIndex.flatMap { case (line, y) => {
        line.toCharArray.toSeq.zipWithIndex.map { case (char, x) => {
          Coord(x, y) -> char
        }
        }
      }
      }
      .filter(_._2 != ' ')
      .toMap

    def paths(aCoord: Coord, bCoord: Coord): Set[Seq[Vector]] = {
      if (aCoord == bCoord) {
        return Set.empty
      }

      def _walk(currentSet: Set[Seq[Coord]]): Set[Seq[Coord]] = {

        if (currentSet.exists(x => x.last == bCoord)) {
          return currentSet.filter(_.last == bCoord)
        }

        val nextPaths = currentSet.flatMap(currentPath => {
          val nexts = currentPath.last.neighbors()
            .filter(points.contains)
            .filterNot(currentPath.contains)
            .toSeq
          nexts.map(n => currentPath :+ n)
        })
        _walk(nextPaths)
      }

      val shortestPoints = _walk(Set(Seq(aCoord)))

      def _convertToVectors(input: Set[Seq[Coord]]): Set[Seq[Vector]] = {
        input.map(coordPath => {
          coordPath.sliding(2, 1).map(pair => {
            val p1 = pair.head
            val p2 = pair.last
            Vector(p2.x - p1.x, p2.y - p1.y)
          }).toSeq
        })
      }

      val fewestStepPaths = _convertToVectors(shortestPoints)
      //how to fitler to lowest number of turns?
      val pathToTurns = fewestStepPaths.map(path => {
        path -> path.sliding(2, 1).count(pair => pair.head != pair.last)
      }).toMap

      val minTurns = pathToTurns.values.min
      pathToTurns.filter(_._2 == minTurns).keySet
      //      fewestStepPaths
    }

    val shortestPaths = points.keys.flatMap(p => points.keys.map(p2 => (p, p2)))
      //      .filter(_._1 != _._2)
      .map { pair => {
        ((points(pair._1), points(pair._2)) -> paths(pair._1, pair._2))
      }
      }.toMap

    shortestPaths
  }

  def getShortestPaths(code: String): Set[String] = {

    val buttonsToHit = Seq('A') ++ code.toCharArray.toSeq

    val paths = buttonsToHit.sliding(2, 1).foldLeft(Set[Seq[Seq[Vector]]]())((acc, nextPair) => {
      val nextSegmentPossibilities = shortestPaths((nextPair.head, nextPair.last))

      val next = if (nextSegmentPossibilities.isEmpty) {
        acc.map(a => a :+ Seq.empty)
      } else {
        nextSegmentPossibilities.flatMap(n => {
          if (acc.isEmpty) then {
            Set(Seq(n))
          } else {
            acc.map(a => a :+ n)
          }
        })
      }

      next
    })

    val result = paths.map(path => {
      path.map(x => x.map(_.toChar).mkString("")).mkString("A") + "A"
    })

    result
  }
}

object NumPad extends Robot {
  private val data: String =
    """789
      |456
      |123
      | 0A""".stripMargin

  def getData(): String = data
}

object ArrowPad extends Robot {
  private val data: String =
    """ ^A
      |<v>""".stripMargin

  def getData(): String = data
}
