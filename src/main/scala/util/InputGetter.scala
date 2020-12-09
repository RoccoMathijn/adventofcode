package util

import os.Path

import scala.annotation.tailrec

object InputGetter {

  sealed trait Mode
  case object Live extends Mode
  case object Example extends Mode

  private val session = sys.env("SESSIONID")

  @tailrec
  def get(day: Int, year: Int, mode: Mode): Seq[String] = {
    val targetPath: Path = os.pwd / "src" / "main" / "resources" / s"aoc$year" / s"input-day$day.txt"
    val examplePath: Path = Path(targetPath.wrapped.getParent) / f"input-day$day-example.txt"

    if (os.exists(targetPath)) {
      mode match {
        case Live    => os.read.lines(targetPath)
        case Example => os.read.lines(examplePath)
      }
    } else {
      println("Downloading day " + day)
      os.write(targetPath, requests.get.stream(s"https://adventofcode.com/$year/day/$day/input", check = true, cookieValues = Map("session" -> session)))
      os.write(examplePath, "")
      get(day, year, mode)
    }
  }
}
