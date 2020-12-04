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
    val maybebyr = map.get("byr")
    val maybeiyr = map.get("iyr")
    val maybeeyr = map.get("eyr")
    val maybehgt = map.get("hgt")
    val maybehcl = map.get("hcl")
    val maybeecl = map.get("ecl")
    val maybepid = map.get("pid")
    val maybecid = map.get("cid")

    val hgtRegex = "(\\d*)(cm|in)".r
    val hclRegex = "#[a-f0-9]{6}"
    val eclRegex = "amb|blu|brn|gry|grn|hzl|oth"
    val pidRegex = "\\d{9}"

    for {
      byr <- maybebyr.filter(b => b.toInt >= 1920 && b.toInt <= 2002)
      iyr <- maybeiyr.filter(i => i.toInt >= 2010 && i.toInt <= 2020)
      eyr <- maybeeyr.filter(b => b.toInt >= 2020 && b.toInt <= 2030)
      hgt <- maybehgt.filter { hgt =>
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
      hcl <- maybehcl.filter(h => h.matches(hclRegex))
      ecl <- maybeecl.filter(e => e.matches(eclRegex))
      pid <- maybepid.filter(p => p.matches(pidRegex))
    } yield {
      Passport(byr, iyr, eyr, hgt, hcl, ecl, pid, maybecid)
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
