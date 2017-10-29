package org.github.sheliaklyr.ebnf

import org.scalatest._

class ParserSuite extends FreeSpec {
  "parse abnf" in {
    val read = io.Source.fromFile("examples/ebnf.ebnf").getLines.mkString("\n")
    Parser.ebnf.parse(read)
  }
}