package aoc2015

import util.AocTools
import util.InputGetter.Live
import java.security.MessageDigest
object Day04 extends AocTools(4, 2015) {
  implicit val mode = Live
  val input = inputLines.head

  def md5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes)
  }

  def convertBytesToHex(bytes: Seq[Byte]): String = {
    val sb = new StringBuilder
    for (b <- bytes) {
      sb.append(String.format("%02x", Byte.box(b)))
    }
    sb.toString
  }

  def findHash(n: Int): Int = {
    if (convertBytesToHex(md5(input + n.toString)).startsWith("000000")) n else findHash(n + 1)
  }

  def main(args: Array[String]): Unit = {
    println(findHash(0))
  }
}
