package com.github.sheliaklyr.ebnf

import org.scalatest.{FreeSpec, MustMatchers}

class GrammarSuite extends FreeSpec with MustMatchers {
  "inline" in {
    import Ebnf._
    val g = Grammar(
      "obj" -> seq(ref("if"), ref("expr"), "then"),
      "if" -> "if",
      "epxr" -> (Special("number") | (ref("expr") ~ ref("op") ~ ref("expr"))),
      "op" -> (stringToTerm("a") | "b" | "c")
    )

    g.inline(Set("if", "op", "expr")) mustBe Grammar(
      "obj" -> seq("if", ref("expr"), "then"),
      "epxr" -> (Special("number") | (ref("expr") ~ (stringToTerm("a") | "b" | "c") ~ ref("expr")))
    )
  }
}
