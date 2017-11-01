package com.github.sheliaklyr.ebnf

case class Rule(rhs: Expr, referredBy: Set[String]) {
  lazy val references: Set[String] = rhs.references
}

case class Grammar(rules: Map[String, Expr]) {
  lazy val rulesSortedByDependencies: Seq[(String, Rule)] = {
    val tr = rules.transform {
      case (n, e) => Rule(e, rules.filter(_._2.references.contains(n)).keySet)
    }
    val ret = tr.toSeq.sortBy(_._2.referredBy.size)
    ret
  }

  /**
    * Removes the provided rules from the grammar by inlining them.
    * Rules that cannot be inlined (recursive, missing) are ignored.
    * Rules that are not used in any expression are removed - it is possible
    * to end with an empty grammar.
    *
    * @param rulesToInline Rules to inline
    */
  def inline(rulesToInline: Set[String]): Grammar = {
    rulesToInline.foldLeft(this)(_ inline _)
  }

  def inline(rule: String): Grammar = {
    rules.get(rule) match {
      case Some(e) if !e.references(rule) =>
        val newRules = (rules - rule).map { case (mn, me) =>
          (mn, me.inline(rule, e))
        }
        Grammar(newRules)
      case _ => this
    }
  }
}

object Grammar {
  def apply(s: (String, Expr)*): Grammar = {
    new Grammar(s.toMap)
  }
}