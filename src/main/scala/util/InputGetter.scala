package util

import os.Path

import scala.annotation.tailrec

object InputGetter {

  sealed trait Mode
  case object Live extends Mode
  case object Example extends Mode

  private val session = sys.env("SESSIONID")

  def get(day: Int, year: Int, mode: Mode): Seq[String] = {
    val targetPath: Path = os.pwd / "src" / "main" / "resources" / s"aoc$year" / s"input-day$day.txt"
    val examplePath: Path = Path(targetPath.wrapped.getParent) / f"input-day$day-example.txt"

    createIfNotExists(targetPath)
    createIfNotExists(examplePath)

    mode match {
      case Live =>
        val lines = os.read.lines(targetPath)
        if (lines.isEmpty) {
          println("Downloading day " + day)
          os.write.over(targetPath, requests.get.stream(s"https://adventofcode.com/$year/day/$day/input", check = true, cookieValues = Map("session" -> session)))
          os.read.lines(targetPath)
        } else lines
      case Example => os.read.lines(examplePath)
    }
  }

  private def createIfNotExists(path: Path) = {
    if (!os.exists(path)) os.write(path, "")
  }
}
