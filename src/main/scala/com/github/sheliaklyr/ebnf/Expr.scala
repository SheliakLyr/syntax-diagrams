package com.github.sheliaklyr.ebnf

/**
 * Represents an EBNF expression.
 */
sealed trait Expr {
  /**
    * Set of all non-terminals present in this expression.
    * @return
    */
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

  /**
    * Replace NonTerminals `ref` by given expression `by`
    * @param ref NonTerminal to replace.
    */
  def inline(ref: String, by: Expr): Expr = mapBottomUp {
    case NonTerminal(x) if x == ref => by
    case otherwise =>
      otherwise
  }

  /**
    * Returns expression simplified by:
    *  - removing unnecessary nested sequences, choices and options
    *  - replacing 1-element seqs/choices by the element itself
    */
  def simplified: Expr = mapBottomUp {
    case ret @ Choice(es) =>
      val got = es.flatMap {
        case Choice(es2) => es2
        case x => Seq(x)
      }
      if (got.size == 1) got.head
      else if (got.size > es.size) Choice(got)
      else ret
    case ret @ Sequence(es) =>
      val got = es.flatMap {
        case Sequence(es2) => es2
        case x => Seq(x)
      }
      if (got.size == 1) got.head
      else if (got.size > es.size) Sequence(got)
      else ret
    case ret @ Optional(o) =>
      o match {
        case _: Optional => o
        case _ => ret
      }
    case otherwise => otherwise
  }

  /**
    * Applies function `f` via recursive `mapBottomUp` on each subexpression,
    * starting from the most nested expressions.
    * @return Mapped expression.
    */
  def mapBottomUp(f: Expr => Expr): Expr
}

object Ebnf {
  import scala.language.implicitConversions
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

  override def mapBottomUp(f: Expr => Expr): Expr = {
    f(copy(es.map(_.mapBottomUp(f))))
  }
}

/**
 * Represents a collection of alternatives (only one can be selected).
 */
case class Choice(es: Seq[Expr]) extends Expr {
  require(es.size >= 2)
  lazy val references: Set[String] = es.map(_.references).reduce(_ ++ _)

  override def mapBottomUp(f: Expr => Expr): Expr = {
    f(copy(es.map(_.mapBottomUp(f))))
  }
}
/**
 * At least once.
 */
case class Repeat(e: Expr) extends Expr {
  def references: Set[String] = e.references

  override def mapBottomUp(f: Expr => Expr): Expr = f(copy(e.mapBottomUp(f)))
}

/**
 * Zero or exactly once.
 */
case class Optional(e: Expr) extends Expr {
  def references: Set[String] = e.references

  override def mapBottomUp(f: Expr => Expr): Expr = f(copy(e.mapBottomUp(f)))
}

/**
 * Common trait for symbols.
 */
sealed trait Symbol extends Expr {
  def value: String

  def references: Set[String] = Set.empty

  override def mapBottomUp(f: Expr => Expr): Expr = f(this)
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


