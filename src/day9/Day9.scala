package day9

import helpers.Helpers

import scala.annotation.tailrec

object Day9 {
  def main(args: Array[String]): Unit = {
    //val input = Helpers.readFile("src/day9/test.txt").head
    //val input = Helpers.readFile("src/day9/test2.txt").head
    val input = Helpers.readFile("src/day9/day9.txt").head

    val hdd = HardDrive.parse(input)

    val compacted = hdd.compact()

    val part1 = HardDrive.score(compacted)
    println(s"Part 1: $part1")

    val compacted2 = hdd.compact2()

    val part2 = HardDrive.score(compacted2)
    println(s"Part 2: $part2")
    
  }
}

trait Section {
  val length: Int
  val fileType: String
}

case class File(fileId: Long, length: Int) extends Section {
  override val fileType: String = this.getClass.getSimpleName

  override def toString: String = {
    (0 until length).toSeq.map(x => fileId).mkString("")
  }
}

case class Empty(length: Int) extends Section {
  override val fileType: String = this.getClass.getSimpleName

  override def toString: String = {
    (0 until length).toSeq.map(x => '.').mkString("")
  }
}

class HardDrive(val sections: Seq[Section]) {

  def compact(): Seq[Section] = {

    @tailrec
    def _compact(current: Seq[Section]): Seq[Section] = {

      val firstEmptyOpt = current.zipWithIndex.find(_._1.fileType=="Empty")

      //end condition
      if(firstEmptyOpt.isEmpty) then return current

      val (firstEmpty,emptyIndex) = firstEmptyOpt.get
      val (lastFile,fileIndex) = current.zipWithIndex.last

      if(firstEmpty.length == lastFile.length) then {
        //direct replacement
        val updatedFiles = current.take(emptyIndex) ++ Seq(lastFile) ++ current.drop(emptyIndex+1).dropRight(1)
        _compact(trimEndingEmpty(updatedFiles))
      } else if firstEmpty.length > lastFile.length then {
        val updatedFiles = current.take(emptyIndex) ++ Seq(lastFile, Empty(firstEmpty.length-lastFile.length)) ++ current.drop(emptyIndex+1).dropRight(1)
        _compact(trimEndingEmpty(updatedFiles))
      } else /* firstEmpty.length < lastFile.length */ {
        val fileId = (lastFile.asInstanceOf[File]).fileId
        val firstSplitFile = File(fileId, firstEmpty.length)
        val lastSplitFile = File(fileId, lastFile.length-firstEmpty.length)
        val updatedFiles =
          current.take(emptyIndex)
          ++ Seq(firstSplitFile)
          ++ current.drop(emptyIndex+1).dropRight(1)
          ++ Seq(lastSplitFile)

        _compact(trimEndingEmpty(updatedFiles))
      }

    }
    _compact(sections)
  }

  def compact2(): Seq[Section] = {

    @tailrec
    def _compact2(current: Seq[Section], movedFiles: Set[Section]): Seq[Section] = {

      val firstEmptyOpt = current.zipWithIndex.find(_._1.fileType == "Empty")
      
      val lastFileAndIndexOpt = current.zipWithIndex.findLast((x,i) => {
        x.isInstanceOf[File] && !movedFiles.contains(x)
      })
      
      if (lastFileAndIndexOpt.isEmpty) then {
        return current //no more files can be moved
      }
      
      val (lastFile, fileIndex) = lastFileAndIndexOpt.map(x => (x._1.asInstanceOf[File],x._2)).get

      val firstEmptyWithIndex = firstEmptyOpt.get

      if (fileIndex < firstEmptyWithIndex._2) {
        //the next file to be moved is already left of the next empty spot
        return current
      }

      val bigEmptyWithIndexOpt = current.zipWithIndex.find(s => s._1.isInstanceOf[Empty] && s._1.length >= lastFile.length).map(x => (x._1.asInstanceOf[Empty],x._2))

      if bigEmptyWithIndexOpt.isEmpty then {
        //can't move this file
        _compact2(current, movedFiles + lastFile)
      } else if (fileIndex < bigEmptyWithIndexOpt.get._2) {
        _compact2(current, movedFiles + lastFile)
      } else {
        //put the file into the empty spot
        val (empty,emptyIndex) = bigEmptyWithIndexOpt.get


        //TODO: replace file's old location with new Empty
        //  there is no reason to compact the empties, as, by definition, no file to the right of the current file
        //  can be moved into those spots
        if(empty.length == lastFile.length) {
          //replace the empty with the file
          val updatedFiles = current.take(emptyIndex) ++ Seq(lastFile) ++ current.drop(emptyIndex+1).map(x => if x == lastFile then Empty(lastFile.length) else x)
          _compact2(trimEndingEmpty(updatedFiles),movedFiles + lastFile)
        } else {
          //fill up as much of the empty as possible
          val updatedFiles = current.take(emptyIndex) ++ Seq(lastFile,Empty(empty.length-lastFile.length)) ++ current.drop(emptyIndex+1).map(x => if x == lastFile then Empty(lastFile.length) else x)
          _compact2(trimEndingEmpty(updatedFiles),movedFiles + lastFile)
        }
      }

    }

    _compact2(sections,Set())
  }

  private def trimEndingEmpty(current: Seq[Section]): Seq[Section] = {

    current.reverse.dropWhile(_.isInstanceOf[Empty]).reverse
  }

}

object HardDrive {
  def parse(input: String): HardDrive = {
    val raw = input.toCharArray.map(c => c.toInt - '0'.toInt).zipWithIndex

    val files: Seq[Section] = raw.map{case (fileLength,fileId) => {
      if fileId%2 == 0 then File(fileId/2,fileLength)
      else Empty(fileLength)
    }}.toSeq.filter(_.length > 0)

    HardDrive(files)
  }

  def score(files: Seq[Section]): Long = {
    files.foldLeft((0L,0)){(acc,next) => {
      val thisFileTotal = (0 until next.length).map(_.toLong + acc._2).map(x => {
        (next match {
          case f: File => f.fileId
          case e: Empty => 0L
        })
        * x
      }).sum
      val nextTotal = acc._1 + thisFileTotal
      val nextStartIndex = acc._2 + next.length
      (nextTotal,nextStartIndex)
    }}._1
  }

}
