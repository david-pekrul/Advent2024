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

    val solutions2 = machines.map(_.getCost(None,Some(10000000000000L)))
    val part2 = solutions2.map(_.getOrElse(0L)).sum
    println(s"Part 2: $part2")
    //76246593362537 too high
    //73267584326867
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

  def getCost(buttonLimit: Option[Long] = Some(100L), addTarget: Option[Long] = None): Option[Long] = {

    val lcmA = Helpers.lcm(Seq(aX,aY))
    val m1 = lcmA / aX;
    val m2 = lcmA / aY;

    val bTop = ((X+addTarget.getOrElse(0L))*m1 - (Y+addTarget.getOrElse(0L))*m2)
    val bBot = (bX*m1 - bY*m2)

    val mod = bTop%bBot

    if mod != 0 then {
      return None
    }

    val B = bTop/bBot

    val A = ((X+addTarget.getOrElse(0L)) - B*bX)/(aX)

    if buttonLimit.isDefined then {
      if A > buttonLimit.get || B > buttonLimit.get then {
        return None
      }
    }

    if (A < 0 || B < 0) then {
      return None
    }
    
    if((A*aX + B*bX) != (X+addTarget.getOrElse(0L))) then {
      return None
    } 
    if((A*aY + B*bY) != (Y+addTarget.getOrElse(0L))) then {
      return None
    }
    val cost = 3*A + B
    return Some(cost)
  }
}
