package com.github.sheliaklyr.ebnf

import java.io.{BufferedOutputStream, File, FileOutputStream}

import com.github.sheliaklyr.ebnf.diagram.SVG
import com.github.sheliaklyr.ebnf.diagram.SVG.Options

import scala.util.Random

object MaxWidthExampleRunner {
  def main(args: Array[String]) {
    val options = Options(
      maxWidth = 500,
      linker = x => "#" + x
    )

    val outputDir: File = new File(new File(new File("examples"), "diagrams"), "maxwidth")
    outputDir.mkdirs()
    val diagrams = wideOption.rulesSortedByDependencies.map {
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
        }: _*
      )
    ).toString()

    val out = new BufferedOutputStream(new FileOutputStream(new File("examples/diagrams/maxwidth/all.html")))
    out.write(page.getBytes("utf-8"))
    out.close()
  }

  import com.github.sheliaklyr.ebnf.Ebnf.{choice, ref, stringToTerm}

  def randomTermsSeq(n: Int): Expr = Sequence(Stream.fill[Expr](n)(stringToTerm(Random.alphanumeric.take(8).mkString(""))))

  val wideOption: Grammar = Grammar(
    "maybe" -> ("test1" ~ randomTermsSeq(10) ~ ("test2".? ~ randomTermsSeq(10)).?)
  )
}
