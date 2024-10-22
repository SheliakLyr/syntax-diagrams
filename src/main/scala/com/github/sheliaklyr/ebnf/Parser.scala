package com.github.sheliaklyr.ebnf

object Parser {
  import fastparse._
  import NoWhitespace._

  private def character[$: P]: P[String] = P(CharIn("a-z").rep(1).! | CharIn("A-Z").rep(1).!)
  private def identifier[$: P]: P[String] = P(character ~/ (character | CharIn("0-9").rep(1).! | CharIn("_").rep(1).!).rep map {
    case (h, ts) => (h + ts.mkString).toLowerCase
  })

  private def whitespace[$: P]: P[Unit] = P(CharsWhileIn(" \r\n").?)

  private def special[$: P]: P[Expr] = P(("?" ~/ whitespace ~ CharsWhile(_ != '?').! ~ whitespace ~ "?").map(x => Special(x.trim)))

  private def simpleExpr[$: P]: P[Expr] = P(NoCut(identifier.map(NonTerminal) | terminal | optional | repeat | special))

  private def optional[$: P]: P[Optional] = P("[" ~/ whitespace ~ expr ~ whitespace ~ "]").map(Optional)

  private def repeat[$: P]: P[Optional] = P("{" ~/ whitespace ~ expr ~ whitespace ~ "}").map(x => Optional(Repeat(x)))

  private def concatExpr[$: P]: P[Expr] = P((simpleExpr ~ (whitespace ~ "," ~/ whitespace ~ simpleExpr).rep).map {
    case (e1, es) if es.nonEmpty => Sequence(e1 +: es)
    case (e1, _) => e1
  })

  private def choiceExpr[$: P]: P[Expr] = P((concatExpr ~ (whitespace ~ "|" ~/ whitespace ~ concatExpr).rep).map {
    case (e1, es) if es.nonEmpty => Choice(e1 +: es)
    case (e1, _) => e1
  })

  private def expr[$: P]: P[Expr] = P(choiceExpr)

  private def terminal[$: P]: P[Terminal] = P((("\"" ~/ CharsWhile(_ != '\"').! ~ "\"") | ("\'" ~/ P(CharsWhile(_ != '\'')).! ~ "\'")).map(x => Terminal(x)))

  private def rule[$: P]: P[(String, Expr)] = P(identifier ~/ whitespace ~ "=" ~ whitespace ~ expr ~ whitespace ~ ";" ~ whitespace)

  /**
   * Ebnf parser
   */
  def ebnf[$: P]: P[Grammar] = P((rule.rep ~ End).map(x => Grammar(x.toMap)))
}