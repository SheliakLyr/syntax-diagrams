package com.github.sheliaklyr.ebnf

/**
 * Represents an EBNF expression.
 */
sealed trait Expr {
  def references: Set[String]
  def | (e: Expr) : Expr = Choice(Seq(this, e))
  def ~ (e: Expr) : Expr = Sequence(Seq(this, e))
  def * : Expr = Optional(Repeat(this))
  def + : Expr = Repeat(this)
  def ? : Expr = Optional(this)
  def rep(occurrences: Int) =
    Sequence(Seq.fill(occurrences)(this))
  def repSepBy(sep: Expr): Expr = (this ~ (sep ~ this).*).?
  def rep1SepBy(sep: Expr): Expr = this ~ (sep ~ this).*
}

object Ebnf {
  import scala.languageFeature.implicitConversions
  implicit def stringToTerm(s: String): Expr = Terminal(s)
  def choice(s: Expr*): Expr = Choice(s)
  def seq(s: Expr*): Expr = Sequence(s)
  def rep(s: Expr): Expr = Optional(Repeat(s))
  def rep1Sep(r: Expr, sep: Expr): Expr =
    seq(r, rep(seq(sep, r)))
  def repSep(r: Expr, sep: Expr): Expr =
    Optional(rep1Sep(r, sep))
  def ref(s: String): Expr = NonTerminal(s)
}

/**
 * Joins expressions in a sequence.
 */
case class Sequence(es: Seq[Expr]) extends Expr {
  require(es.size >= 2)
  lazy val references: Set[String] = es.map(_.references).reduce(_ ++ _)
}

/**
 * Represents a collection of alternatives (only one can be selected).
 */
case class Choice(es: Seq[Expr]) extends Expr {
  require(es.size >= 2)
  lazy val references: Set[String] = es.map(_.references).reduce(_ ++ _)
}
/**
 * At least once.
 */
case class Repeat(e: Expr) extends Expr {
  def references: Set[String] = e.references
}

/**
 * Zero or exactly once.
 */
case class Optional(e: Expr) extends Expr {
  def references: Set[String] = e.references
}

/**
 * Common trait for symbols.
 */
sealed trait Symbol extends Expr {
  def value: String

  def references: Set[String] = Set.empty
}

/**
 * Represents a non-terminal symbol (reference to a rule).
 */
case class NonTerminal(value: String) extends Symbol {
  override lazy val references: Set[String] = Set(value)
}

/**
 * Terminal symbol, for example 'a', 'term' etc.
 */
case class Terminal(value: String) extends Symbol

/**
 * Special kind of terminal, for example textual description, regex spec etc.
 */
case class Special(value: String) extends Symbol


