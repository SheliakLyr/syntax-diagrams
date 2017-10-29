package org.github.sheliaklyr.ebnf.diagram

/**
 * 2D Vector
 */
case class Vec(x: Int, y: Int) {
  def dy(d: Int): Vec = Vec(x, y + d)
  def dx(d: Int): Vec = Vec(x + d, y)
  def + (v: Vec): Vec = Vec(x + v.x, y + v.y)
}

object Vec {
  val zero: Vec = Vec(0,0)
}
