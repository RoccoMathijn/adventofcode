package aoc2022

import util.AocTools
import util.InputGetter.{Live, Mode}

object Day07 extends AocTools(7, 2022) {
//  implicit private val mode: Mode = Example
  implicit private val mode: Mode = Live

  val input: Seq[Command] = inputLines.foldLeft(List.empty[Command]) { (acc, line) =>
    if (line.startsWith("$")) line match {
      case s"$$ cd $argument" => acc :+ CD(argument)
      case s"$$ ls"           => acc :+ LS(List.empty, List.empty)
    }
    else {
      line match {
        case s"dir $name" =>
          val lastLs = acc.last.asInstanceOf[LS]
          acc.init :+ lastLs.copy(dirs = lastLs.dirs :+ Dir(name))
        case s"$size $name" =>
          val lastLs = acc.last.asInstanceOf[LS]
          acc.init :+ lastLs.copy(files = lastLs.files :+ File(name, size.toInt))
      }
    }
  }

  sealed trait Command
  case class CD(argument: String) extends Command
  case class LS(files: List[File], dirs: List[Dir]) extends Command

  case class Dir(name: String, subDirs: List[Dir] = List.empty, files: List[File] = List.empty)
  case class File(name: String, size: Int)

  def fileTree(commands: Seq[Command], currDir: Dir): (Seq[Command], Dir) = {
    if (commands.isEmpty) (Seq.empty, currDir)
    else {
      commands.head match {
        case CD(argument) =>
          argument match {
            case ".." => (commands.tail, currDir)
            case dirName =>
              val (newCommands, newDir) = fileTree(commands.tail, Dir(dirName))
              val newSubDirs = currDir.subDirs.filterNot(_.name == dirName) :+ newDir
              fileTree(newCommands, currDir.copy(subDirs = newSubDirs))
          }
        case LS(files, dirs) =>
          fileTree(commands.tail, Dir(currDir.name, subDirs = dirs, files = files))
      }
    }
  }

  val root = fileTree(input.tail, currDir = Dir("/"))._2

  def recursiveFiles(dir: Dir): List[File] = {
    dir match {
      case Dir(_, List(), files) => files
      case Dir(_, subDirs, files) =>
        files ++ subDirs.flatMap(recursiveFiles)
    }
  }

  def dirsWithSizes(root: Dir): List[(Dir, Int)] = {
    val files: Int = root.files.map(_.size).sum
    val subFiles: Int = root.subDirs.flatMap(recursiveFiles).map(_.size).sum
    (root, files + subFiles) :: root.subDirs.flatMap(dirsWithSizes)
  }

  val dirsWithSizesList: List[(Dir, Int)] = dirsWithSizes(root)

  val part1 = dirsWithSizesList.filter(_._2 <= 100000).map(_._2).sum

  val rootSize = recursiveFiles(root).map(_.size).sum
  val deleteMinimal = 30000000 - (70000000 - rootSize)
  val part2 = dirsWithSizesList.sortBy(_._2).find(_._2 > deleteMinimal).get._2

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    println(s"AOC 2022 - Day $day")

    val mid = System.currentTimeMillis()
    println(s"Answer part 1: $part1 [${mid - start}ms]")
    val end = System.currentTimeMillis()
    println(s"Answer part 2: $part2 [${end - mid}ms]")
  }
}
