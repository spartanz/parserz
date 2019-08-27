package org.spartanz.parserz

import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

import scala.annotation.tailrec

class SimplestExampleV2Spec extends Specification {

  object Syntax {
    sealed trait Expression
    case class Constant(value: Int)                extends Expression
    case class Sum(e1: Expression, e2: Expression) extends Expression
  }

  object Example {

    object Parser extends ParsersModule {
      override type Input = String
    }

    type S = Unit
    type E = String

    import Syntax._
    import Parser._
    import Parser.Grammar._

    val char: Grammar[Any, Nothing, E, Char] = consumeOptional0[E, Char]("expected: char")(
      s => s.headOption.map(s.drop(1) -> _),
      { case (s, c) => Some(s + c.toString) }
    )

    val digit: Grammar[Any, Nothing, E, Char]        = char.filter("expected: digit")(_.isDigit)
    val plus: Grammar[Any, Nothing, E, Char]         = char.filter("expected: '+'")(_ == '+')
    val integer: Grammar[Any, Nothing, E, Int]       = digit.map(_.toString.toInt, _.toString.head)
    val constant: Grammar[Any, Nothing, E, Constant] = integer.map(Constant, _.value)

    val expr1: Grammar[Any, Nothing, E, Expression] = constant.mapPartial("expected: Constant")(
      { case c               => c },
      { case c @ Constant(_) => c }
    )

    val expr2: Grammar[Any, Nothing, E, Expression] = (expr1 ~ (plus ~ expr1).rep).map(
      (fold2 _).tupled,
      unfold2('+')(Nil)
    )

    // todo: generalize fold/unfold

    def fold2[D](z: Expression, list: List[(D, Expression)]): Expression =
      list.foldLeft(z) { case (e1, (_, e2)) => Sum(e1, e2) }

    @tailrec
    def unfold2[D](separator: D)(acc: List[(D, Expression)])(e: Expression): (Expression, List[(D, Expression)]) =
      e match {
        case c @ Constant(_) => (c, acc)
        case _ @Sum(e1, e2)  => unfold2(separator)((separator, e2) :: acc)(e1)
      }

    val parser: (S, Input) => (S, E \/ (Input, Expression))  = Parser.parser[S, E, Expression](expr2)
    val printer: (S, (Input, Expression)) => (S, E \/ Input) = Parser.printer[S, E, Expression](expr2)
  }

  import Syntax._

  private def parse(s: String)  = Example.parser((), s)._2
  private def parse0(s: String) = parse(s).right.get._2

  private def print(e: Expression)  = Example.printer((), ("", e))._2
  private def print0(e: Expression) = print(e).right.get

  private def loop0(s: String, e: Expression): MatchResult[Any] = {
    val parsed  = parse0(s)
    val printed = print0(parsed)
    (parsed must_=== e).and(printed must_=== s)
  }

  "empty" in {
    parse("") must_=== Left("expected: char")
  }
  "single letter" in {
    parse("A") must_=== Left("expected: digit")
  }
  "single digit" in {
    loop0("1", Constant(1))
  }
  "several digits" in {
    parse("12") must_=== Right(("2", Constant(1)))
  }
  "just the plus" in {
    parse("+") must_=== Left("expected: digit")
  }
  "sum of two" in {
    loop0("1+2", Sum(Constant(1), Constant(2)))
  }
  "sum of three" in {
    loop0("1+2+3", Sum(Sum(Constant(1), Constant(2)), Constant(3)))
  }
  "sum of four" in {
    loop0("1+2+3+4", Sum(Sum(Sum(Constant(1), Constant(2)), Constant(3)), Constant(4)))
  }
  "incorrect composed sum of three" in {
    print(Sum(Constant(1), Sum(Constant(2), Constant(3)))) must_=== Left("expected: Constant")
  }
}
