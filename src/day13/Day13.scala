package day13

import helpers.Helpers

object Day13 {
  def main(args: Array[String]): Unit = {
//    val input = Helpers.readFile("src/day13/test.txt").filter(!_.trim.isBlank)
    val input = Helpers.readFile("src/day13/day13.txt").filter(!_.trim.isBlank)

    val machines = ClawMachine.parse(input)

    val solutions = machines.map(_.getCost())

    val part1 = solutions.map(_.getOrElse(0L)).sum
    println(s"Part 1: $part1")
  }
}

object ClawMachine {

  val buttonRegex = """Button \w: X\+(\d+), Y\+(\d+)""".r
  val prizeRegex = """Prize: X=(\d+), Y=(\d+)""".r

  def parse(input: Seq[String]): Seq[ClawMachine] = {
    val rawMachines = input.sliding(3,3).toSeq

    rawMachines.map(rawMachine => {
      val buttonRegex(aX, aY) = rawMachine(0)
      val buttonRegex(bX, bY) = rawMachine(1)
      val prizeRegex(totalX, totalY) = rawMachine(2)

      ClawMachine(aX.toInt, aY.toInt, bX.toInt, bY.toInt, totalX.toInt, totalY.toInt)
    })
  }
}

class ClawMachine(val aX: Long, val aY: Long, val bX: Long, val bY: Long, val X: Long, val Y: Long) {

  def getCost(): Option[Long] = {

    val lcmA = Helpers.lcm(Seq(aX,aY))
    val m1 = lcmA / aX;
    val m2 = lcmA / aY;

    val bTop = (X*m1 - Y*m2)
    val bBot = (bX*m1 - bY*m2)

    val mod = bTop%bBot

    if mod != 0 then {
      return None
    }

    val B = bTop/bBot
    
    val A = (X - B*bX)/(aX)

    if A > 100 || B > 100 then {
      return None
    }
    
    if (A < 0 || B < 0) then {
      return None
    }
    return Some(3*A + B)
  }
}
