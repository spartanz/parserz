package org.spartanz.parserz

trait Enumerable[A] {

  val min: A
  val max: A
  def range(min: A, max: A): Iterable[A]
}
