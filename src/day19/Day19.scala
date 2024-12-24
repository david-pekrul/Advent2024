import helpers.Helpers

object Day19 {
  def main(args: Array[String]): Unit = {
    //val input = Helpers.readFile("src/day19/test.txt")
    val input = Helpers.readFile("src/day19/day19.txt")

    val (towels, patterns) = parse(input)

    val part1data = patterns.map(p => (p -> canMakePattern(p, towels)))
    val part1 = part1data.count(_._2)
    println(s"Part 1: $part1")


    val part2data = part1data.filter(_._2).map(_._1).map(p => (p -> countPatterns(p, towels)))
    val part2 = part2data.toMap.values.sum
    println(s"Part 2: $part2")

  }

  def parse(input: Seq[String]): (Seq[Towel], Seq[ColorPattern]) = {
    val towels = input(0).split(",").toSeq.map(_.trim.toCharArray.toSeq).map(Towel.apply)

    val patterns = input.drop(2).toSeq.map(_.trim.toCharArray.toSeq).map(ColorPattern.apply)
    (towels, patterns)
  }

  def canMakePattern(p: ColorPattern, towels: Seq[Towel]): Boolean = {

    val towelsToConsider = towels.filter(t => p.colors.containsSlice(t.colors))

    def _findPattern(remaining: Seq[Char]): Boolean = {

      if (remaining.isEmpty) then {
        return true
      }

      val applicableTowels = towelsToConsider.filter(t => {
        remaining.startsWith(t.colors)
      })

      if (applicableTowels.isEmpty) then {
        return false
      }

      applicableTowels.exists(nextTowel => {
        _findPattern(remaining.drop(nextTowel.colors.size))
      })
    }

    _findPattern(p.colors)
  }

  def countPatterns(p: ColorPattern, towels: Seq[Towel]): Long = {

    val towelsToConsider = towels.filter(t => p.colors.containsSlice(t.colors))

    val startMap = p.colors.indices.map(idx => idx -> 0L).toMap.updated(0, 1L)

    val data = p.colors.indices.foldLeft(startMap)((currentMap, idx) => {
      val currentCountToGetToIndex = currentMap(idx)
      towelsToConsider.foldLeft(currentMap)((updatedMap, nextTowel) => {
        if (p.colors.drop(idx).startsWith(nextTowel.colors)) then {
          val nextMap = updatedMap.updatedWith(idx + nextTowel.colors.length)(existing => Some(existing.getOrElse(0L) + currentCountToGetToIndex))
          nextMap
        } else {
          updatedMap
        }
      })
    })


    data(p.colors.length)
  }
}

case class Towel(colors: Seq[Char]) {

}

case class ColorPattern(colors: Seq[Char]) {

}