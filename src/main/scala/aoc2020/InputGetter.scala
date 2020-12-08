package aoc2020

import scala.annotation.tailrec

object InputGetter {

  private val session = sys.env("SESSIONID")

  @tailrec
  def get(day: Int): Seq[String] = {
    val target = os.pwd / "src" / "main" / "resources" / "aoc2020" / f"input-day$day.txt"
    if (os.exists(target))
      os.read.lines(target)
    else {
      println("Downloading day " + day)
      os.write(target, requests.get.stream(s"https://adventofcode.com/2020/day/$day/input", check = true, cookieValues = Map("session" -> session)))

      get(day)
    }

  }

}