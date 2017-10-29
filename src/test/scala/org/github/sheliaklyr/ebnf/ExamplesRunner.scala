package org.github.sheliaklyr.ebnf

import java.io.{File, FileWriter}

import org.github.sheliaklyr.ebnf.diagram.SVG
import org.github.sheliaklyr.ebnf.diagram.SVG.Options

object ExamplesRunner {
  def main(args: Array[String]) {
    val options = Options(
      maxWidth = 900,
      embeddedStyle = SVG.defaultStyle(),
      linker = x => "#" + x
    )

    val outputDir: File = new File(new File(new File("examples"), "diagrams"), "json")
    outputDir.mkdirs()
    jsonEbnf.rulesSortedByDependencies.foreach {
      case (name, rule) =>
        val tag = SVG.createDiagram(rule.rhs, options)
        val content = tag.render
        val outputFile = new File(outputDir, name + ".svg")
        val fileWriter = new FileWriter(outputFile)
        try {
          fileWriter.append(content)
        } finally {
          fileWriter.close()
        }
    }
  }

  import Ebnf.{seq, choice, repSep, stringToTerm, ref}

  val jsonEbnf: Grammar = Grammar(
    "json" -> ref("object").|(ref("array")),
    "digitP" -> Special("[1-9]"),
    "digit" -> Special("[0-9]"),
    "hexDigit" -> Special("[0-9A-Fa-f]"),
    "string" -> {
        val unicode = Special("Any UNICODE char except: \" \\ or control character")
        val escaped = "\\" ~ choice(
          "\"",
          "\\",
          "/",
          "b",
          "f",
          "n",
          "r",
          "t",
          "u" ~ ref("hexDigit").rep(4)
        )
        "\"" ~ (unicode | escaped).* ~ "\""
    },
    "number" -> {
      val beforeDot = "0" | (ref("digitP") ~ ref("digit").*)
      val exp = ("E" | "e") ~ ("-" | "+").? ~ ref("digit").+
      "-".? ~ beforeDot ~ ("." ~ ref("digit").+).? ~ exp.?
    },
    "value" -> choice(
      ref("object"),
      ref("array"),
      ref("string"),
      ref("number"),
      "true",
      "false",
      "null"
    ),
    "pair" -> ref("string") ~ ":" ~ ref("value"),
    "object" -> "{" ~ ref("pair").repSepBy(",") ~ "}",
    "array" -> "[" ~ ref("value").repSepBy(ref("value")) ~ "]"
  )
}