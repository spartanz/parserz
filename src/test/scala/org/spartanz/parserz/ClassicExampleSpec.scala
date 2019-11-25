package org.spartanz.parserz

import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

object ClassicExampleSpec {

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

    import Parser.Expr._
    import Parser.Grammar._
    import Parser._
    import Syntax._

    type S    = Unit
    type E    = String
    type G[A] = Grammar[Any, Nothing, E, A]

    private val `(` = '('
    private val `)` = ')'

    val char: G[Char] = "char" @@ consumeOption("expected: char")(
      s => s.headOption.map(s.drop(1) -> _),
      { case (s, c) => Some(s + c.toString) }
    )

    val digit: G[Char]  = char.filter("expected: digit")(_.isDigit).tag("digit")
    val paren1: G[Char] = char.filterExpr("expected: open paren")(===(`(`))
    val paren2: G[Char] = char.filterExpr("expected: close paren")(===(`)`))

    val plus: G[Operator] = "+" @@ char.mapPartial("expected: '+'")(
      { case '+' => Add },
      { case Add => '+' }
    )

    val star: G[Operator] = "*" @@ char.mapOption("expected: '*'")(
      { case '*' => Some(Mul); case _ => None },
      { case Mul => Some('*'); case _ => None }
    )

    val integer: G[Int] = "integer" @@ digit.rep1.map(
      chars => chars.mkString.toInt,
      int => { val chars = int.toString.toList; ::(chars.head, chars.tail) }
    )

    val constant: G[Expression] = "Constant" @@ integer.mapPartial("expected: Constant")(
      { case i           => Constant(i) },
      { case Constant(i) => i }
    )

    val multiplier: G[Expression] = "Multiplier" @@ (((paren1, `(`) ~> addition <~ ((`)`, paren2))) | constant).map({
      case Left(exp)  => SubExpr(exp)
      case Right(exp) => exp
    }, {
      case SubExpr(exp) => Left(exp)
      case exp          => Right(exp)
    })

    val multiplication: G[Expression] = "Multiplication" @@ {
      (constant ~ (star ~ multiplier).rep).foldLeft(
        { case (e1, (_, e2))          => Operation(e1, Mul, e2) },
        { case Operation(e1, Mul, e2) => (e1, (Mul, e2)) }
      )
    }

    lazy val addition: G[Expression] = "Addition" @@ delay {
      (multiplication ~ (plus ~ multiplication).rep).foldLeft(
        { case (e1, (_, e2))          => Operation(e1, Add, e2) },
        { case Operation(e1, Add, e2) => (e1, (Add, e2)) }
      )
    }

    val parser: (S, Input) => (S, E \/ (Input, Expression))  = Parser.parser[S, E, Expression](addition)
    val printer: (S, (Input, Expression)) => (S, E \/ Input) = Parser.printer[S, E, Expression](addition)
  }

}

class ClassicExampleSpec extends Specification {
  import ClassicExampleSpec._
  import Syntax._

  private def parse(s: String)  = Example.parser((), s)._2
  private def parse0(s: String) = parse(s).toOption.get._2

  private def print(e: Expression)  = Example.printer((), ("", e))._2
  private def print0(e: Expression) = print(e).toOption.get

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
    print(Operation(Constant(1), Add, SubExpr(Constant(2)))) must_=== Left("expected: Constant")
  }
  "incorrect composition 3" in {
    print(SubExpr(Constant(1))) must_=== Left("expected: Constant")
  }

  "Docs" should {
    "be available for all grammars 0" in {
      Example.Parser.bnf(Example.Parser.Grammar.unit.tag("const")) must_=== Nil
    }
    "be available for all grammars 1" in {
      Example.Parser.bnf(
        Example.Parser.Grammar
          .consumeStatefully[Int, Int, String, Any](
            (s, _) => (s, Left("a test")),
            (s, _) => (s, Left("a test"))
          )
          .tag("failed")
      ) must_=== Nil
    }
    "be available for all grammars 2" in {
      Example.Parser.bnf(Example.integer).mkString("\n", "\n", "\n") must_===
        """
          |<digit> ::= <char>
          |<integer> ::= NEL(<digit>)
          |""".stripMargin
    }
    "be available for all grammars 3" in {
      Example.Parser.bnf(Example.addition).mkString("\n", "\n", "\n") must_===
        """
          |<digit> ::= <char>
          |<integer> ::= NEL(<digit>)
          |<Constant> ::= <integer>
          |<*> ::= <char>
          |<Multiplier> ::= ("(" <Addition> ")" | <Constant>)
          |<Multiplication> ::= <Constant> List(<*> <Multiplier>)
          |<+> ::= <char>
          |<Addition> ::= <Multiplication> List(<+> <Multiplication>)
          |""".stripMargin
    }
  }
}
