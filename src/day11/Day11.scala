package day11

import helpers.Helpers

import scala.annotation.tailrec

object Day11 {
  def main(args: Array[String]): Unit = {
    //val input = Helpers.readFile("src/day11/test.txt").head
    val input = Helpers.readFile("src/day11/day11.txt").head

    val stones = Stone.parse(input)
    val stoneCollection = StoneCollection.build(stones)
    
    val afterBlinks = stoneCollection.blink(25)
    val part1 = afterBlinks.stoneLedger.values.sum
    println(s"Part 1: $part1")


    val afterBlinks2 = stoneCollection.blink(75)
    val part2 = afterBlinks2.stoneLedger.values.sum
    println(s"Part 2: $part2")
  }
}

object StoneCollection {
  def build(stones: Seq[Stone]): StoneCollection = {
    StoneCollection(stones.groupBy(x => x).map(kv => (kv._1, kv._2.size.toLong)))
  }
}

case class StoneCollection(stoneLedger: Map[Stone,Long]) {
  //For each stone, this is how many instances there are

  def blink(blinks: Int): StoneCollection = {

    @tailrec
    def _blink(current: StoneCollection, remainingBlinks: Int): StoneCollection = {
      if (remainingBlinks == 0) then return current

      val nextStep = current.stoneLedger.toSeq.flatMap{ case (stone,count) => {
        stone.op().map(newStone => (newStone,count))
      }}

      val updated = nextStep.groupMap(_._1)(_._2).map{ case (stone,allCounts) => (stone,allCounts.sum)}
      val newCollection = StoneCollection(updated)
      _blink(newCollection, remainingBlinks - 1)
    }

    _blink(this,blinks)
  }

  override def toString: String = {
    "\t" + this.stoneLedger.mkString("\r\n\t")
  }
}

object Stone {
  def parse(input: String): Seq[Stone] = {
    input.split(" ").map(_.toLong).map(Stone.apply).toSeq
  }
}

case class Stone(value: Long) {

  lazy val numDigits = Math.log10(value).intValue+1

  def op(): Seq[Stone] = {
    if value == 0 then {
      Seq(Stone(1))
    } else if numDigits%2 == 0 then {
      val powTen = Math.pow(10,(numDigits/2)).longValue
      Seq(Stone(value/powTen),Stone(value%powTen))
    } else {
      Seq(Stone(value*2024))
    }
  }
}
