package org.spartanz.parserz

import org.specs2.mutable.Specification

object SeparatedBySpec {

  object Parser extends ParsersModule {
    override type Input = String
  }

  import Parser._
  import Parser.Expr._
  import Parser.Grammar._

  type S = Unit
  type E = String
  type G[A] = Grammar[Any, Nothing, E, A]

  val char: G[Char] = consumeOption("no input")(
    s => s.headOption.map(s.drop(1) -> _),
    { case (s, c) => Some(s + c.toString) }
  )

  val separator1: G[Char] = char.filter("expected: '-'")(===('-'))
  val separator2: G[Char] = char.filter("expected: '+'")(===('+'))

  val separator: G[Char] = (separator1 | separator2).mapPartial("wrong separator")(
    { case s => s.merge },
    { case '-' => Left('-'); case '+' => Right('+') }
  )
  val sequence: G[SeparatedBy[Char, Char]] = char.filter("expected: [A-D]")(in('A', 'B', 'C', 'D')).separated(separator)

  def parse[A](g: G[A]): Input => E \/ (Input, A) = parser[S, E, A](g)((), _)._2
  def print[A](g: G[A]): A => E \/ Input          = a => printer[S, E, A](g)((), ("", a))._2
}

class SeparatedBySpec extends Specification {
  import SeparatedBySpec._

  "separated by '-'" should {
    "->" in {
      parse(sequence)("") must_=== Right(("", SeparatedBy()))
      parse(sequence)("E") must_=== Right(("E", SeparatedBy()))
      parse(sequence)("A") must_=== Right(("", SeparatedBy('A')))
      parse(sequence)("A-") must_=== Right(("-", SeparatedBy('A')))
      parse(sequence)("A-E") must_=== Right(("-E", SeparatedBy('A')))
      parse(sequence)("A-B") must_=== Right(("", SeparatedBy('A', '-', SeparatedBy('B'))))
      parse(sequence)("A+B") must_=== Right(("", SeparatedBy('A', '+', SeparatedBy('B'))))
      parse(sequence)("A-B-C") must_=== Right(("", SeparatedBy('A', '-', SeparatedBy('B', '-', SeparatedBy('C')))))
    }

    "<-" in {
      print(sequence)(SeparatedBy()) must_=== Right("")
      print(sequence)(SeparatedBy('A')) must_=== Right("A")
      print(sequence)(SeparatedBy('E')) must_=== Left("expected: [A-D]")
      print(sequence)(SeparatedBy('A', '-', SeparatedBy('B'))) must_=== Right("A-B")
      print(sequence)(SeparatedBy('A', '+', SeparatedBy('B'))) must_=== Right("A+B")
      print(sequence)(SeparatedBy('A', '?', SeparatedBy('B'))) must_=== Left("wrong separator")
      print(sequence)(SeparatedBy('A', '-', SeparatedBy('E'))) must_=== Left("expected: [A-D]")
      print(sequence)(SeparatedBy('E', '-', SeparatedBy('A'))) must_=== Left("expected: [A-D]")
      print(sequence)(SeparatedBy('A', '-', SeparatedBy('E', '-', SeparatedBy('C')))) must_=== Left("expected: [A-D]")
      print(sequence)(SeparatedBy('A', '-', SeparatedBy('B', '-', SeparatedBy('C')))) must_=== Right("A-B-C")
    }
  }

  "extracted separators" in {
    val res1 = parse(sequence)("A-B+C").toOption.get._2
    res1.separators must_=== List('-', '+')
    val res2 = parse(sequence)("A+B-C").toOption.get._2
    res2.separators must_=== List('+', '-')
  }
}
