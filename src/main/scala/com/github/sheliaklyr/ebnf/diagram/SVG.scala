package com.github.sheliaklyr.ebnf.diagram

import com.github.sheliaklyr.ebnf
import com.github.sheliaklyr.ebnf._

import scalatags.generic
import scalatags.text.Builder

object SVG {

  import scalatags.Text.all._
  import scalatags.Text.{TypedTag, svgAttrs => A, svgTags => S}

  private type Tag = TypedTag[String]
  private type Tags = List[Tag]

  private def translate(x: Int, y: Int, t: TypedTag[String]*) = {
    S.g(A.transform := s"translate($x,$y)")(t)
  }

  /**
    * @param i y-component of input
    * @param o y-component of output
    */
  private case class SubGraph(
     i: Int,
     o: Int,
     size: Vec,
     elements: Seq[TypedTag[String]]
   ) {
    def width: Int = size.x

    def height: Int = size.y

    val belowInput: Int = height - i
    val belowOutput: Int = height - o

    def inputsDiff: Int = o - i

    def translated(x: Int, y: Int): TypedTag[String] =
      translate(x, y, elements: _*)
  }

  private object CssClass {
    type A = generic.AttrPair[Builder, String]
    val path: A = `class` := "path"
    val nterm: A = `class` := "nterm"
    val special: A = `class` := "special"
    val term: A = `class` := "term"
    val triangle: A = `class` := "triangle"
    val startCircle: A = `class` := "circleS"
    val endCircle: A = `class` := "circleE"
  }

  private sealed trait HorizontalDirection
  private sealed trait VerticalDirection
  private object Right extends HorizontalDirection
  private object Left extends HorizontalDirection
  private object Up extends VerticalDirection
  private object Down extends VerticalDirection

  private case class PathBuilder(d: List[String]) {
    def add(s: String): PathBuilder = copy(s :: d)
    def M(x: Int, y: Int): PathBuilder = add(s"M $x $y")
    def m(x: Int, y: Int): PathBuilder = add(s"m $x $y")
    def L(x: Int, y: Int): PathBuilder = add(s"L $x $y")
    def l(x: Int, y: Int): PathBuilder = add(s"l $x $y")
    def h(x: Int): PathBuilder = add(s"h $x")
    def v(y: Int): PathBuilder = add(s"v $y")
    def z: PathBuilder = add("z")

    def curve(h: HorizontalDirection, v: VerticalDirection): PathBuilder = (h, v) match {
      case (Right, Up) => cd(10, 0, 14, -4, 14, -14)
      case (Right, Down) => cd(10, 0, 14, 4, 14, 14)
      case (Left, Up) => cd(-10, 0, -14, -4, -14, -14)
      case (Left, Down) => cd(-10, 0, -14, 4, -14, 14)
    }

    def curve(v: VerticalDirection, h: HorizontalDirection): PathBuilder = (h, v) match {
      case (Right, Up) => cd(0, -10, 4, -14, 14, -14)
      case (Right, Down) => cd(0, 10, 4, 14, 14, 14)
      case (Left, Up) => cd(0, -10, -4, -14, -14, -14)
      case (Left, Down) => cd(0, 10, -4, 14, -14, 14)
    }

    private def cd(xc: Int, yc: Int, xc2: Int, yc2: Int, xd: Int, yd: Int) = {
      add(s"c $xc $yc $xc2 $yc2 $xd $yd")
    }

    def toPath: Tag = S.path(
      CssClass.path,
      A.fill := "none",
      A.strokeWidth := 2,
      A.d := d.reverse.mkString(" ")
    )
  }

  private def path(): PathBuilder = PathBuilder(Nil)

  private val fontWidth = 7
  private val segmentSize = 7
  private val lineHeight = segmentSize * 4
  private val halfLine = segmentSize * 2
  private val textBoxSidePadding = fontWidth
  private val textLengthAttr = A.attr("textLength")

  private def textWidth(s: String): Int = s.length * fontWidth

  private def textBoxWidth(s: String): Int = textWidth(s) + 2 * textBoxSidePadding

  private def generateSymbolElements(s: ebnf.Symbol)(implicit options: Options) = {
    def textNode(sv: String) = S.text(
      A.x := fontWidth,
      A.y := 18,
      A.fontFamily := "monospace",
      A.fontSize := "12px",
      A.fill := "white",
      textLengthAttr := s"${sv.length * fontWidth}px",
      sv)
    s match {
      case NonTerminal(ntValue) =>
        options.linker(ntValue) match {
          case "" =>
            Seq(
              S.rect(
                A.width := textBoxWidth(ntValue),
                A.height := lineHeight,
                CssClass.nterm),
              textNode(options.showNonTerm(s.value))
            )
          case linkTo =>
            Seq(
              a(
                attr("xlink:href") := "#" + ntValue,
                CssClass.nterm,
                S.rect(
                  A.width := textBoxWidth(ntValue),
                  A.height := lineHeight,
                  CssClass.nterm),
                textNode(options.showNonTerm(s.value))
              )
            )
        }
      case Special(sValue) =>
        val d = lineHeight / 2 - 3
        val w = textBoxWidth(sValue)
        Seq(
          S.polygon(
            CssClass.special,
            A.points := s"0,$d $d,0 ${w - d},0 $w,$d $w,${lineHeight - d} ${w - d},$lineHeight $d,$lineHeight 0, ${lineHeight - d}"),
          textNode(s.value)
        )
      case other =>
        Seq(
          S.rect(
            CssClass.term,
            A.rx := segmentSize * 3.0 / 2,
            A.ry := segmentSize * 3.0 / 2,
            A.width := textBoxWidth(s.value),
            A.height := lineHeight),
          textNode(s.value)
        )
    }
  }

  private def mapWithState[A, S, B](source: List[A], initialState: S)(f: (A, S) => (B, S)): List[B] = {
    source.foldLeft((initialState, List.empty[B])) {
      case ((s, l), x) =>
        val (b, newS) = f(x, s)
        (newS, b :: l)
    }._2.reverse
  }

  private def generateSubGraph(expr: Expr)(implicit options: Options): SubGraph = expr match {
    case s: ebnf.Symbol if s.value.isEmpty => // not shown
      SubGraph(
        halfLine,
        halfLine,
        Vec(0, lineHeight),
        Nil
      )

    case s: ebnf.Symbol =>
      SubGraph(
        halfLine,
        halfLine,
        Vec(textBoxWidth(s.value), lineHeight),
        generateSymbolElements(s)
      )

    // optimized optional alternative
    case Optional(Choice(es)) =>
      generateSubGraph(Choice(Special("") +: es))

    case Choice(ese) =>
      val es = ese.map(generateSubGraph(_)(decreasedMaxWidthByWrapper))
      val h = es.map(_.height).sum + ((es.size - 1) * segmentSize)
      val maxWidthElem = es.map(_.width).max
      val w = maxWidthElem + 8 * segmentSize
      case class Vecitioned(x: Int, y: Int, e: SubGraph)
      val (_, res) = es.foldLeft((0, List.empty[Vecitioned])) {
        case ((pos, acc), sg@SubGraph(_, _, Vec(_, lh), el)) =>
          (pos + lh + 7, Vecitioned(28, pos, sg) :: acc)
      }
      val height = es.view.init.map(_.height).sum + ((es.size - 1) * segmentSize) - lineHeight
      val mainPath = path()
        .m(0, 14)
        .h(w)
        .curve(Left, Down)
        .v(height)
        .M(0, 14)
        .curve(Right, Down)
        .v(height)
        .toPath

      val alternativePaths = res.init.map { p =>
        path()
          .M(p.x - 14, p.y)
          .v(p.e.i - 14)
          .curve(Down, Right)
          .M(w - 14, p.y)
          .v(p.e.o - 14)
          .curve(Down, Left)
          .h(-(maxWidthElem - p.e.width))
          .toPath
      }

      val elements = res.map { p => p.e.translated(p.x, p.y) }

      val els = mainPath :: alternativePaths ::: elements
      SubGraph(14, 14, Vec(w, h), els)

    // optimized rep1SepBy
    case Sequence(Seq(base: ebnf.Symbol, Optional(Repeat(Sequence(Seq(sep: ebnf.Symbol, base_repeated: ebnf.Symbol)))), rest@_*)) if base == base_repeated =>
      val repeater = {
        val baseGraph = generateSubGraph(base)
        val sepGraph = generateSubGraph(sep)
        val w = (baseGraph.width max sepGraph.width) + 8 * segmentSize
        val h = (sepGraph.height + sepGraph.height) + segmentSize
        val els = List(
          path()
            .m(0, 14)
            .h(28)
            .curve(Left, Down)
            .v(baseGraph.height - 28 + 7)
            .curve(Down, Right)
            .h(w - 7 * 8)
            .curve(Right, Up)
            .v(28 - baseGraph.height - 7)
            .curve(Up, Left)
            .h(28)
            .z.toPath,
          baseGraph.translated(28, 0),
          sepGraph.translated(28, 35)
        )
        SubGraph(14, 14, Vec(w, h), els)
      }

      if (rest.isEmpty) repeater
      else {
        val restGraph = generateSubGraph(if (rest.size == 1) rest.head else Sequence(rest))

        val subGraphs = List(repeater, restGraph)

        val w = subGraphs.map(_.width).sum + ((subGraphs.size - 1) * segmentSize)
        val (_, res) = subGraphs.foldLeft((0, List.empty[Tag])) {
          case ((pos, acc), sg@SubGraph(_, _, Vec(lw, _), el)) =>
            (pos + lw + 7, sg.translated(pos, 0) :: acc)
        }
        val els = path().m(0, 14).h(w).toPath :: res.reverse
        val (aboveL, belowL) = subGraphs.map { e => (e.belowInput, e.i) }.unzip
        val resultHeight = aboveL.max + belowL.max
        SubGraph(14, 14, Vec(w, resultHeight), els)
      }


    case Sequence(es) =>
      val expressionsWithSubGraphs = es.map(x => (x, generateSubGraph(x)(decreasedMaxWidthByWrapper)))
      val subGraphs = expressionsWithSubGraphs.map(_._2)
      val totalW = subGraphs.map(_.width).sum + ((subGraphs.size - 1) * segmentSize)

      def line(graphs: List[SubGraph]) = {
        val w = graphs.map(_.width).sum + ((graphs.size - 1) * segmentSize)
        val totalDiff = graphs.map(_.inputsDiff).sum
        val (_, maxAbove) = graphs.foldLeft((14, 14)) {
          case ((pos, acc), sg) =>
            (pos + sg.inputsDiff, acc max (pos max sg.i))
        }
        val input = (maxAbove - graphs.head.i) max graphs.head.i
        val (_, resultHeight) = graphs.foldLeft((input, 0)) {
          case ((prevOutput, h), sg) =>
            (prevOutput + sg.inputsDiff, h max (prevOutput + sg.belowInput))
        }

        val res = mapWithState[SubGraph, Vec, Seq[Tag]](graphs, Vec(0, input)) {
          case (sg@SubGraph(_, _, Vec(lw, _), el), pos) =>
            val newS = pos + Vec(lw + 7, sg.inputsDiff)
            val movedElement = sg.translated(pos.x, pos.y - sg.i)
            def connection = path().M(pos.x, pos.y).h(-segmentSize).toPath
            val els = if (pos.x == 0) Seq(movedElement)
            else Seq(connection, movedElement)
            (els, newS)
        }.flatten

        SubGraph(input, input + totalDiff, Vec(w, resultHeight), res)
      }

      def place[X, T](graphs: List[X])(f: (Vec, X) => (Vec, T)) = {
        graphs.foldLeft((Vec(0, 0), List.empty[T])) {
          case ((pos, acc), sg) =>
            val (newVec, accHead) = f(pos, sg)
            (newVec, accHead :: acc)
        }._2.reverse
      }

      if (totalW < options.maxWidth) {
        line(subGraphs.toList)
      } else {
        val lines = subGraphs.foldLeft[(Int, List[List[SubGraph]])]((0, List(List.empty[SubGraph]))) {
          case ((pos, ah :: at), sg) =>
            val diff = sg.width + 7
            if (pos + diff > options.maxWidth - 2 * 28) (diff, List(sg) :: ah :: at)
            else (pos + diff, (sg :: ah) :: at)
        }._2.filter(_.nonEmpty).map(sgs => line(sgs.reverse)).reverse

        val linesSize = lines.size
        val maxLineWidth = lines.map(_.width).max

        val entry = path().m(0, 14).h(28).toPath

        val output = lines.tail.init.map(_.height).sum + ((lines.size - 2) * 28) + lines.last.o + lines.head.belowOutput

        val linesElements = place(lines.zipWithIndex) {
          case (pos, (sg, idx)) =>
            val joinNext =
              if (idx + 1 == linesSize) path()
                .m(pos.x + 28 + sg.width, pos.y + sg.o)
                .h(maxLineWidth - sg.width + 14)
                .curve(Right, Up)
                .v(- output)
                .curve(Up, Right)
                .toPath
              else path()
                .m(pos.x + 28 + sg.width, pos.y + sg.o)
                .curve(Right, Down)
                .v(sg.height - sg.o - sg.i)
                .curve(Down, Left)
                .h(-sg.width)
                .curve(Left, Down)
                .curve(Down, Right)
                .toPath
            (pos + Vec(0, sg.height + 28), List(joinNext, sg.translated(pos.x + 28, pos.y)))
        }.flatten

        SubGraph(
          14, 14, Vec(maxLineWidth + 28 * 2 + 14, lines.map(_.height).sum + (lines.size * 28 - 28)),
          entry :: linesElements
        )
      }


    // optimized * for single element
    case Optional(Repeat(s: ebnf.Symbol)) =>
      val e = generateSubGraph(s)(decreasedMaxWidthByWrapper)
      val w = e.width + 8 * segmentSize
      val h = e.height + 4 * segmentSize
      val els = List(
        path()
          .m(0, 14)
          .h(28)
          .curve(Left, Down)
          .curve(Down, Right)
          .h(w - 7 * 8)
          .curve(Right, Up)
          .curve(Up, Left)
          .h(28)
          .z
          .toPath,
        e.translated(28, 28)
      )
      SubGraph(14, 14, Vec(w, h), els)

    case Optional(es) =>
      generateSubGraph(Choice(Seq(Special(""), es)))

    case Repeat(es) =>
      val e = generateSubGraph(es)(decreasedMaxWidthByWrapper)
      val w = e.width + 8 * segmentSize
      val h = e.height + 4 * segmentSize
      val els = List(
        path()
          .m(0, 28 + e.i)
          .h(28)
          .curve(Left, Up)
          .v(-e.i + 14)
          .curve(Up, Right)
          .h(w - 7 * 8)
          .curve(Right, Down)
          .v(e.i - 14)
          .curve(Down, Left)
          .h(28)
          .z.toPath,
        e.translated(28, 28)
      )
      SubGraph(28 + e.i, 28 + e.o, Vec(w, h), els)
  }

  case class Options(
    maxWidth: Int = 1000,
    cssClass: String = "ebnf",
    embeddedStyle: String = "",
    linker: String => String = _ => "",
    showNonTerm: String => String = identity)

  def defaultStyle(rootClass: String = "ebnf"): String = {
    s"""
       |svg a.nterm:hover {
       |  text-decoration: underline;
       |}
       |
       |svg rect.term {
       |  fill: lightgrey;
       |  stroke: black;
       |  stroke-width: 2;
       |}
       |
       |svg polygon.special {
       |  fill: lightgrey;
       |  stroke: black;
       |  stroke-width: 2;
       |}
       |
       |svg rect.nterm {
       |  fill: white;
       |  stroke: black;
       |  stroke-width: 2;
       |}
       |
       |svg .path {
       |  stroke: black;
       |  stroke-width: 2;
       |}
       |
       |svg text {
       |  fill: black;
       |  font-weight: bold;
       |}
       |
       |svg .circleS {
       |  fill: none;
       |  stroke: black;
       |  stroke-width: 2;
       |}
       |
       |svg .circleE {
       |  fill: "black";
       |  stroke: black;
       |  stroke-width: 2;
       |}
       |""".stripMargin.trim
  }

  private def decreasedMaxWidthByWrapper(implicit options: Options) = decreasedMaxWidth(segmentSize * 8)

  private def decreasedMaxWidth(pixels: Int)(implicit options: Options) = options.copy(maxWidth = options.maxWidth - pixels)

  def createDiagram(rhs: Expr, options: Options = Options()): Tag = {
    // remove unneeded elements
    val simplified = rhs.simplified
    val elements = generateSubGraph(simplified)(decreasedMaxWidth(62)(options))

    def triangle(posx: Int, posy: Int) = S.polygon(
      A.points := s"$posx ${posy - 5}, $posx ${posy + 5}, ${posx + 10} $posy",
      CssClass.triangle
    )

    val result = S.svg(
      attr("xmlns") := "http://www.w3.org/2000/svg",
      attr("xmlns:xlink") := "http://www.w3.org/1999/xlink",
      // plus 10 for additional css rules (stroke width)
      `class` := options.cssClass,
      style := s"width: ${elements.width + 62}px; height: ${elements.height + 10}px;",
      Option(options.embeddedStyle).filter(_.nonEmpty).map(x => scalatags.Text.tags2.style(x)).getOrElse(raw("")),
      translate(0, 5,
        triangle(15, elements.i),
        S.circle(
          A.cx := 4,
          A.cy := elements.i,
          A.r := 3,
          CssClass.startCircle),
        path().m(7, elements.i).h(28).toPath,
        triangle(elements.width + 31 + 9, elements.o),
        S.circle(
          A.cx := elements.width + 27 + 31,
          A.cy := elements.o,
          A.r := 3,
          CssClass.endCircle),
        path().m(elements.width + 31, elements.o).h(28).toPath,
        translate(31, 0, elements.elements: _*)
      )
    )
    result
  }
}
