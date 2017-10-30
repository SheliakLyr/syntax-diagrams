package com.github.sheliaklyr.ebnf

import java.io.{BufferedOutputStream, File, FileOutputStream}

import com.github.sheliaklyr.ebnf.diagram.SVG
import com.github.sheliaklyr.ebnf.diagram.SVG.Options

object ExamplesRunner {
  def main(args: Array[String]) {
    val options = Options(
      maxWidth = 900,
      linker = x => "#" + x
    )

    val outputDir: File = new File(new File(new File("examples"), "diagrams"), "json")
    outputDir.mkdirs()
    val diagrams = jsonEbnf.rulesSortedByDependencies.map {
      case (name, rule) => name -> SVG.createDiagram(rule.rhs, options)
    }

    import scalatags.Text.all._
    val page = html(
      head(
        scalatags.Text.tags2.style(
          SVG.defaultStyle()
        )
      ),
      body(
        diagrams.flatMap { case (n, diagram) =>
          Seq(
            h1(n, id := n),
            diagram
          )
        }:_*
      )
    ).toString()

    val out = new BufferedOutputStream(new FileOutputStream(new File("examples/diagrams/json/all.html")))
    out.write(page.getBytes("utf-8"))
    out.close()
  }

  import com.github.sheliaklyr.ebnf.Ebnf.{choice, ref, stringToTerm}

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