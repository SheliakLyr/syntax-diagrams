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
}

object Grammar {
  def apply(s: (String, Expr)*): Grammar = {
    new Grammar(s.toMap)
  }
}