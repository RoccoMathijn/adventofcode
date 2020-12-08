package aoc2020

import os.Path

import scala.annotation.tailrec

object InputGetter {

  sealed trait Mode
  case object Live extends Mode
  case object Example extends Mode

  private val session = sys.env("SESSIONID")

  @tailrec
  def get(day: Int, mode: Mode): Seq[String] = {
    val target: Path = os.pwd / "src" / "main" / "resources" / "aoc2020" / f"input-day$day.txt"
    val examplePath = Path(target.wrapped.getParent) / f"input-day$day-example.txt"
    if (os.exists(target)) {
      mode match {
        case Live    => os.read.lines(target)
        case Example => os.read.lines(examplePath)
      }
    } else {
      println("Downloading day " + day)
      os.write(target, requests.get.stream(s"https://adventofcode.com/2020/day/$day/input", check = true, cookieValues = Map("session" -> session)))
      os.write(examplePath, "")
      get(day, mode)
    }
  }
}
