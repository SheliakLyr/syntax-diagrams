package com.github.sheliaklyr.ebnf

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class ParserSuite extends AnyFreeSpec with Matchers {
  "parse abnf" in {
    val read: String = io.Source.fromFile("examples/ebnf.ebnf").getLines.mkString("\n")
    fastparse.parse(read, Parser.ebnf(_)).get
  }
}