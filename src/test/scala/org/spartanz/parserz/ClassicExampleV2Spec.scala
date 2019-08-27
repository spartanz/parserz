package org.spartanz.parserz

import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

import scala.annotation.tailrec

class ClassicExampleV2Spec extends Specification {

  object Syntax {
    sealed trait Expression
    case class Constant(value: Int)                                    extends Expression
    case class Operation(e1: Expression, op: Operator, e2: Expression) extends Expression
    case class SubExpr(e: Expression)                                  extends Expression

    sealed trait Operator
    case object Add extends Operator
    case object Mul extends Operator
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

    val digit: Grammar[Any, Nothing, E, Char]  = "digit" @@ char.filter("expected: digit")(_.isDigit)
    val paren1: Grammar[Any, Nothing, E, Char] = "(" @@ char.filter("expected: open paren")(_ == '(')
    val paren2: Grammar[Any, Nothing, E, Char] = ")" @@ char.filter("expected: close paren")(_ == ')')

    val plus: Grammar[Any, Nothing, E, Operator] = "+" @@ char.mapPartial("expected: '+'")(
      { case '+' => Add },
      { case Add => '+' }
    )

    val star: Grammar[Any, Nothing, E, Operator] = "*" @@ char.mapPartial("expected: '*'")(
      { case '*' => Mul },
      { case Mul => '*' }
    )

    val integer: Grammar[Any, Nothing, E, Int] = "integer" @@ digit.rep1.map(
      chars => chars.mkString.toInt,
      int => { val chars = int.toString.toList; ::(chars.head, chars.tail) }
    )

    val constant: Grammar[Any, Nothing, E, Expression] = "Constant" @@ integer.mapPartial("expected: Constant")(
      { case i           => Constant(i) },
      { case Constant(i) => i }
    )

    val multiplier: Grammar[Any, Nothing, E, Expression] = "Multiplier" @@ ((paren1 ~ addition ~ paren2) | constant).map({
      case Left(((_, exp), _)) => SubExpr(exp)
      case Right(exp)          => exp
    }, {
      case SubExpr(exp) => Left((('(', exp), ')'))
      case exp          => Right(exp)
    })

    val multiplication: Grammar[Any, Nothing, E, Expression] = "Multiplication" @@ {
      (constant ~ (star ~ multiplier).rep)
        .mapOptional("wrong order of ops (hm...)")(
          arg => Some((fold2 _).tupled(arg)),
          unfold2(Mul)(Nil)
        )
    }

    lazy val addition: Grammar[Any, Nothing, E, Expression] = "Addition" @@ delay {
      (multiplication ~ (plus ~ multiplication).rep)
        .mapOptional("wrong order of ops (hmm...)")(
          arg => Some((fold2 _).tupled(arg)),
          unfold2(Add)(Nil)
        )
    }


    // todo: generalize fold/unfold

    private def fold2(z: Expression, list: List[(Operator, Expression)]): Expression =
      list.foldLeft(z) { case (e1, (op, e2)) => Operation(e1, op, e2) }

    @tailrec
    private def unfold2(
      op: Operator
    )(acc: List[(Operator, Expression)])(e: Expression): Option[(Expression, List[(Operator, Expression)])] = e match {
      case c @ Constant(_)                       => Some((c, acc))
      case _ @SubExpr(_)                         => None
      case o @ Operation(_, op2, _) if op2 != op => Some((o, acc))
      case _ @Operation(e1, `op`, e2)            => unfold2(op)((op, e2) :: acc)(e1)
    }


    val parser: (S, Input) => (S, E \/ (Input, Expression))  = Parser.parser[S, E, Expression](addition)
    val printer: (S, (Input, Expression)) => (S, E \/ Input) = Parser.printer[S, E, Expression](addition)
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
    loop0("12", Constant(12))
  }
  "just the plus" in {
    parse("+") must_=== Left("expected: digit")
  }
  "sum of two" in {
    loop0("1+2", Operation(Constant(1), Add, Constant(2)))
  }
  "sum of three" in {
    loop0("1+2+3", Operation(Operation(Constant(1), Add, Constant(2)), Add, Constant(3)))
  }
  "parse sum until it cannot anymore" in {
    parse("1+2++3") must_=== Right(("++3", Operation(Constant(1), Add, Constant(2))))
  }
  "parse mul until it cannot anymore" in {
    parse("1*2**3") must_=== Right(("**3", Operation(Constant(1), Mul, Constant(2))))
  }
  "mul of two" in {
    loop0("1*2", Operation(Constant(1), Mul, Constant(2)))
  }
  "mul of three" in {
    loop0("1*2*3", Operation(Operation(Constant(1), Mul, Constant(2)), Mul, Constant(3)))
  }
  "mul of two with parens" in {
    loop0("1*(2)", Operation(Constant(1), Mul, SubExpr(Constant(2))))
  }
  "mix of three" in {
    loop0("12*34+56", Operation(Operation(Constant(12), Mul, Constant(34)), Add, Constant(56)))
  }
  "mix of four (implicit precedence)" in {
    loop0("12+34*56+78", Operation(Operation(Constant(12), Add, Operation(Constant(34), Mul, Constant(56))), Add, Constant(78)))
  }
  "mix of four (explicit precedence)" in {
    loop0("12+34*(56+78)", Operation(Constant(12), Add, Operation(Constant(34), Mul, SubExpr(Operation(Constant(56), Add, Constant(78))))))
  }
  "incorrect composition" in {
    print(Operation(Constant(1), Add, Operation(Constant(2), Add, Constant(3)))) must_=== Left("expected: Constant")
  }
  "incorrect composition 2" in {
    print(Operation(Constant(1), Add, SubExpr(Constant(2)))) must_=== Left("wrong order of ops (hm...)")
  }
  "incorrect composition 3" in {
    print(SubExpr(Constant(1))) must_=== Left("wrong order of ops (hmm...)")
  }
}
