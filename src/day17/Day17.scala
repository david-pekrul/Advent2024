package day17

import helpers.Helpers

import scala.annotation.tailrec

object Day17 {
  def main(args: Array[String]): Unit = {
   
    val testCases = Seq(
      ComputerState.execute(ComputerState(0,0,9,Program(Seq((2,6)),0),Seq.empty)),
      ComputerState.execute(ComputerState(10,0,0,Program(Seq((5,0),(5,1),(5,4)),0),Seq.empty)),
      ComputerState.execute(ComputerState(2024,0,0,Program(Seq((0,1),(5,4),(3,0)),0),Seq.empty)),
      ComputerState.execute(ComputerState(0,29,0,Program(Seq((1,7)),0),Seq.empty)),
      ComputerState.execute(ComputerState(0,2024,43690,Program(Seq((4,0)),0),Seq.empty))
    )

    testCases.foreach(t => {
      println("Test: " + ComputerState.execute(t))
    })
   
    //val input = Helpers.readFile("src/day17/test.txt")
    val input = Helpers.readFile("src/day17/day17.txt")
    val computer = ComputerState.parse(input)
    val part1 = ComputerState.execute(computer).out.mkString(",")
    println(s"Part 1: $part1")
  }
}

object ComputerState {
  val numberRegex = """\D+(\d+)""".r

  def parse(input: Seq[String]): ComputerState = {
    val numberRegex(a) = input(0)
    val numberRegex(b) = input(1)
    val numberRegex(c) = input(2)
    val ops = input(4).replace("Program: ", "").split(",").map(_.toInt).sliding(2, 2).toSeq.map(x => (x(0), x(1)))
    ComputerState(a.toLong, b.toLong, c.toLong, Program(ops, 0), Seq.empty)
  }

  def execute(start: ComputerState): ComputerState = {

    @tailrec
    def _process(currentState: ComputerState): ComputerState = {
      if (currentState.p.isDone()) {
        return currentState
      }

      val operationOpt = currentState.p.getCurrent()
      val (operation, combo) = operationOpt.get
      val comboValue = currentState.getComboValue(combo)

      val updatedComputer = operation match {
        case 0 => {
          val newA = currentState.a / Math.pow(2L, comboValue).longValue
          ComputerState(newA, currentState.b, currentState.c, currentState.p.increment(), currentState.out)
        }
        case 1 => {
          val newB = currentState.b ^ combo
          ComputerState(currentState.a, newB, currentState.c, currentState.p.increment(), currentState.out)
        }
        case 2 => {
          val newB = comboValue % 8
          ComputerState(currentState.a, newB, currentState.c, currentState.p.increment(), currentState.out)
        }
        case 3 => {
          if (currentState.a == 0) {
            ComputerState(currentState.a, currentState.b, currentState.c, currentState.p.increment(), currentState.out)
          } else {
            ComputerState(currentState.a, currentState.b, currentState.c, currentState.p.setPtr(combo), currentState.out)
          }
        }
        case 4 => {
          val newB = currentState.b ^ currentState.c
          ComputerState(currentState.a, newB, currentState.c, currentState.p.increment(), currentState.out)
        }
        case 5 => {
          val outValue = comboValue % 8
          ComputerState(currentState.a, currentState.b, currentState.c, currentState.p.increment(), currentState.out :+ outValue)
        }
        case 6 => {
          val newB = currentState.a / Math.pow(2L, comboValue).longValue
          ComputerState(currentState.a, newB, currentState.c, currentState.p.increment(), currentState.out)
        }
        case 7 => {
          val newC = currentState.a / Math.pow(2L, comboValue).longValue
          ComputerState(currentState.a, currentState.b, newC, currentState.p.increment(), currentState.out)
        }
      }

      _process(updatedComputer)
    }

    _process(start)
  }
}

class ComputerState(val a: Long, val b: Long, val c: Long, val p: Program, val out: Seq[Long]) {
  def getComboValue(c: Long): Long = {
    c match {
      case 0 | 1 | 2 | 3 => c
      case 4 => this.a
      case 5 => this.b
      case 6 => this.c
      case 7 => c //hack?
      case _ => throw new RuntimeException(s"Unknown Combo: $c")
    }
  }

  override def toString: String = {
    s"ComputerState{A:$a; B:$b, C:$c, O:[${out.mkString(",")}]}"
  }
}


/*
The operations and the combo operand
The index of the currently referenced operation
 */
case class Program(ops: Seq[(Int, Int)], optPtr: Int) {
  def getCurrent(): Option[(Int, Int)] = {
    ops.lift(optPtr)
  }

  def increment(): Program = {
    Program(ops, optPtr + 1)
  }

  def setPtr(ptr: Int): Program = {
    Program(ops, ptr)
  }

  def isDone(): Boolean = {
    optPtr >= ops.size
  }
}

object ThreeBitConverter {
  def tenToThree(input: Int): Int = {

    @tailrec
    def _rec(input: Int, built: Seq[Int]): Seq[Int] = {
      if (input <= 0) then {
        return built.reverse
      }
      val remainder = input % 3
      val whole = input / 3
      _rec(whole, built :+ remainder)
    }

    _rec(input, Seq()).mkString("").toInt
  }

  def threeToTen(input: Int): Int = {
    input.toString.toCharArray.reverse.zipWithIndex
      .map(x => (x._1.toInt, x._2))
      .map(x => x._1 * Math.pow(10, x._2).intValue)
      .sum
  }
}