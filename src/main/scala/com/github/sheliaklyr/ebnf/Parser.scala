package com.github.sheliaklyr.ebnf

object Parser {
  import fastparse.all._

  private val character: P[String] = P(CharIn('a' to 'z').rep(1).! | CharIn('A' to 'Z').rep(1).!)
  private val identifier: P[String] = P(character ~/ (character | CharIn('0' to '9').rep(1).! | CharIn("_").rep(1).!).rep map {
    case (h, ts) => (h + ts.mkString).toLowerCase
  })

  private val whitespace = P(CharsWhileIn(" \r\n").?)

  private val special: P[Expr] = P(("?" ~/ whitespace ~ CharsWhile(_ != '?').! ~ whitespace ~ "?").map(x => Special(x.trim)))

  private def simpleExpr: P[Expr] = P(NoCut(identifier.map(NonTerminal) | terminal | optional | repeat | special))

  private def optional = P("[" ~/ whitespace ~ expr ~ whitespace ~ "]").map(Optional)

  private def repeat = P("{" ~/ whitespace ~ expr ~ whitespace ~ "}").map(x => Optional(Repeat(x)))

  private def concatExpr: P[Expr] = P((simpleExpr ~ (whitespace ~ "," ~/ whitespace ~ simpleExpr).rep).map {
    case (e1, es) if es.nonEmpty => Sequence(e1 +: es)
    case (e1, _) => e1
  })

  private def choiceExpr: P[Expr] = P((concatExpr ~ (whitespace ~ "|" ~/ whitespace ~ concatExpr).rep).map {
    case (e1, es) if es.nonEmpty => Choice(e1 +: es)
    case (e1, _) => e1
  })

  private def expr: P[Expr] = P(choiceExpr)

  private def terminal = P((("\"" ~/ CharsWhile(_ != '\"').! ~ "\"") | ("\'" ~/ P(CharsWhile(_ != '\'')).! ~ "\'")).map(x => Terminal(x)))

  private val rule: P[(String, Expr)] = P(identifier ~/ whitespace ~ "=" ~ whitespace ~ expr ~ whitespace ~ ";" ~ whitespace)

  /**
   * Ebnf parser
   */
  val ebnf: P[Grammar] = P((rule.rep ~ End).map(x => Grammar(x.toMap)))
}