package aoc2020

import scala.io.Source

object Day04 extends App {
  val input: Seq[String] = Source
    .fromResource("aoc2020/input-day4.txt")
    .getLines()
    .toList

  case class Passport(
      byr: String,
      iyr: String,
      eyr: String,
      hgt: String,
      hcl: String,
      ecl: String,
      pid: String,
      cid: Option[String]
  )

  def toPassport(input: String): Option[Passport] = {
    val map = input.split(" ").map(kv => kv.split(":")).map(kv => kv(0) -> kv(1)).toMap
    val maybeByr = map.get("byr")
    val maybeIyr = map.get("iyr")
    val maybeEyr = map.get("eyr")
    val maybeHgt = map.get("hgt")
    val maybeHcl = map.get("hcl")
    val maybeEcl = map.get("ecl")
    val maybePid = map.get("pid")
    val maybeUid = map.get("cid")

    val hgtRegex = "(\\d*)(cm|in)".r
    val hclRegex = "#[a-f0-9]{6}"
    val eclRegex = "amb|blu|brn|gry|grn|hzl|oth"
    val pidRegex = "\\d{9}"

    for {
      byr <- maybeByr.filter(b => b.toInt >= 1920 && b.toInt <= 2002)
      iyr <- maybeIyr.filter(i => i.toInt >= 2010 && i.toInt <= 2020)
      eyr <- maybeEyr.filter(b => b.toInt >= 2020 && b.toInt <= 2030)
      hgt <- maybeHgt.filter { hgt =>
        hgt match {
          case hgtRegex(height, unit) =>
            unit match {
              case "cm" if height.toInt >= 150 && height.toInt <= 193 => true
              case "in" if height.toInt >= 59 && height.toInt <= 76   => true
              case _                                                  => false
            }
          case _ => false
        }
      }
      hcl <- maybeHcl.filter(h => h.matches(hclRegex))
      ecl <- maybeEcl.filter(e => e.matches(eclRegex))
      pid <- maybePid.filter(p => p.matches(pidRegex))
    } yield {
      Passport(byr, iyr, eyr, hgt, hcl, ecl, pid, maybeUid)
    }

  }

  def splitLines(input: Seq[String]): Seq[String] = {
    input.mkString("\n").split("\n\n")
  }

  val x = splitLines(input)
    .filterNot(line => line.isEmpty)
    .map(line => line.replaceAll("\n", " "))
    .flatMap(toPassport)
    .size

  println(x)
}
