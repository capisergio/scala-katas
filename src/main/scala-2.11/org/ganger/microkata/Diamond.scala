package org.ganger.microkata

case class Diamond(char: Char) {

  val init = 'A'
  val range = (init to char)
  val size = range.size

  def build: Seq[String] = buildMirror(for (currentChar <- range) yield buildLine(currentChar))

  def buildLine(currentChar: Char): String = {
    val start = for (x <- 0 to size - 1)
      yield if (x == range.reverse.indexOf(currentChar)) currentChar
      else "-"
    (start ++ start.reverse.tail).mkString("")
  }

  def buildMirror(chars: Seq[String]): Seq[String] = chars ++ chars.reverse.tail
}
