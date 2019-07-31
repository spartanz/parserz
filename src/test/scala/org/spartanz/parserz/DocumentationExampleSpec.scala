package org.spartanz.parserz

import org.specs2.mutable.Specification
import org.spartanz.parserz.tc.Category
import scalaz.std.either._

class DocumentationExampleSpec extends Specification {

  object Syntax {
    sealed trait Expression
    case class Constant(value: Int)                                    extends Expression
    case class Operation(e1: Expression, op: Operator, e2: Expression) extends Expression
    case class SubExpr(e: Expression)                                  extends Expression

    sealed trait Operator
    case object Add extends Operator
    case object Mul extends Operator
  }

  object env {
    import Category._

    val parsing: Parsing[Either[String, ?], Either[String, ?], String] = Parsing()
    val equiv: parsing.Equiv.type                                      = parsing.Equiv
    val codec: parsing.Codec.type                                      = parsing.Codec
    val syntax: parsing.syntax.type                                    = parsing.syntax

    type Equiv[A, B] = parsing.Equiv[A, B]
  }

  abstract class Grammar[P[_]: env.parsing.ParserOps] {
    import env.Equiv
    import env.equiv._
    import env.syntax._
    import Syntax._
    import TCInstances.naturalTransformationLoop

    val constantEq: Equiv[Int, Constant] =
      lift(Constant, _.value)

    val constantExpressionEq: Equiv[Constant, Expression] =
      liftPartial("Expected: Constant")(
        { case a               => a },
        { case n @ Constant(_) => n }
      )

    def operationExpressionEq(
      op: Operator
    ): Equiv[Expression /\ (Operator /\ Expression), Expression] =
      liftPartial("Expected: Operation")(
        { case (e1, (`op`, e2))        => Operation(e1, op, e2) },
        { case Operation(e1, `op`, e2) => e1 -> (op -> e2) }
      )

    def char: P[Char]
    def delay[A](pa: => P[A]): P[A]

    val digit: P[Char]  = "digit" @@ (char ∘ ensure("Expected: [0-9]")(_.isDigit))
    val paren1: P[Char] = "(" @@ (char ∘ ensure("expected: open paren")(_ == '('))
    val paren2: P[Char] = ")" @@ (char ∘ ensure("expected: close paren")(_ == ')'))

    val plus: P[Operator] = "+" @@ (char ∘ liftPartial("Expected: '+'")(
      { case '+' => Add }, { case Add => '+' }
    ))

    val star: P[Operator] = "*" @@ (char ∘ liftPartial("Expected: '*'")(
      { case '*' => Mul }, { case Mul => '*' }
    ))

    // todo: how to get this error?
    val integer: P[Int] = "integer" @@ (digit.many1("Expected at least one digit") ∘ lift(
      chars => new String(chars.toArray).toInt,
      int => int.toString.toList
    ))

    val constant: P[Constant] = integer ∘ constantEq

    val constExpr: P[Expression] = "Constant" @@ (constant ∘ constantExpressionEq)

    val multiplier: P[Expression] = "Multiplier" @@ (
      ((paren1 ~ addition ~ paren2) | constExpr) ∘ lift({
        case Left(((_, exp), _)) => SubExpr(exp)
        case Right(exp)          => exp
      }, {
        case SubExpr(exp) => Left((('(', exp), ')'))
        case exp          => Right(exp)
      })
    )

    // todo: how to get this error?
    val multiplication: P[Expression] = "Multiplication" @@ (
      (constExpr ~ (star ~ multiplier).many) ∘ foldl("hm...")(
        operationExpressionEq(Mul)
      )
    )

    lazy val addition: P[Expression] = "Addition" @@ delay {
      (multiplication ~ (plus ~ multiplication).many) ∘ foldl("hmm..")(
        operationExpressionEq(Add)
      )
    }

    lazy val expression: P[Expression] = "Expression" @@ addition
  }

  object Parsers {
    import env.equiv
    import env.codec
    import env.parsing.Codec
    import env.parsing.Codec._

    object Parser extends Grammar[Codec[List[Char], ?]] {
      override def char: Codec[List[Char], Char] = Codec(
        equiv.liftF(
          { case c :: cs => Right(cs -> c); case Nil => Left("Empty input") },
          { case (cs, c) => Right(cs :+ c) }
        )
      )

      override def delay[A](pa: => Codec[List[Char], A]): Codec[List[Char], A] =
        codec.delay(pa)
    }

    import cfg._

    implicit private val descOps: env.parsing.ParserOps[CFGP] = CFGP.parserOps(env.parsing)

    object Desc extends Grammar[CFGP] {
      override def char: CFGP[Char]                  = CFGP(Input("char"))
      override def delay[A](pa: => CFGP[A]): CFGP[A] = CFGP(Delay("", () => pa.cfg))
    }
  }

  "Docs" should {
    "be available for combinators" in {
      Parsers.Desc.integer.show.mkString("\n", "\n", "\n") must_===
        """
          |<digit> ::= <char>
          |<integer> ::= NEL(<digit>)
          |""".stripMargin
    }
    "be available for all expression" in {
      Parsers.Desc.expression.show.mkString("\n", "\n", "\n") must_===
        """
          |<+> ::= <char>
          |<)> ::= <char>
          |<Addition> ::= <Multiplication> List(<+> <Multiplication>)
          |<(> ::= <char>
          |<Multiplier> ::= (<(> <Addition> <)> | <Constant>)
          |<*> ::= <char>
          |<digit> ::= <char>
          |<integer> ::= NEL(<digit>)
          |<Constant> ::= <integer>
          |<Multiplication> ::= <Constant> List(<*> <Multiplier>)
          |<Expression> ::= <Multiplication> List(<+> <Multiplication>)
          |""".stripMargin
    }
  }

  def parse(s: String): Either[String, (String, Syntax.Expression)] =
    Parsers.Parser.expression.parse(s.toCharArray.toList).map {
      case (cs, res) => cs.mkString -> res
    }

  def print(exp: Syntax.Expression): Either[String, String] =
    Parsers.Parser.expression.print(exp, Nil).map(_.mkString)

  "Parser" should {
    import Syntax._

    "not parse empty input" in {
      parse("") must_=== Left("Empty input")
    }
    "parse a digit into a literal" in {
      parse("5") must_=== Right("" -> Constant(5))
    }
    "parse several digits" in {
      parse("567") must_=== Right("" -> Constant(567))
    }

    "not parse a letter and indicate failure" in {
      parse("A") must_=== Left("Expected: [0-9]")
    }
    "not parse '+' by itself" in {
      parse("+") must_=== Left("Expected: [0-9]")
    }

    "parse operation with 2 numbers" in {
      parse("5+6") must_=== Right("" -> Operation(Constant(5), Add, Constant(6)))
      parse("5*6") must_=== Right("" -> Operation(Constant(5), Mul, Constant(6)))
    }
    "parse operation with 3 numbers" in {
      parse("5+6+7") must_=== Right(
        "" -> Operation(Operation(Constant(5), Add, Constant(6)), Add, Constant(7))
      )
      parse("5*6*7") must_=== Right(
        "" -> Operation(Operation(Constant(5), Mul, Constant(6)), Mul, Constant(7))
      )
    }

    "parse till it can" in {
      parse("1**2") must_=== Right("**2"   -> Constant(1))
      parse("1*2**3") must_=== Right("**3" -> Operation(Constant(1), Mul, Constant(2)))
    }

    "parse expressions" in {
      parse("12*34+56") must_=== Right(
        "" -> Operation(Operation(Constant(12), Mul, Constant(34)), Add, Constant(56))
      )
      parse("12+34*56") must_=== Right(
        "" -> Operation(Constant(12), Add, Operation(Constant(34), Mul, Constant(56)))
      )
      parse("1*2+3*4") must_=== Right(
        "" -> Operation(
          Operation(Constant(1), Mul, Constant(2)),
          Add,
          Operation(Constant(3), Mul, Constant(4))
        )
      )
    }

    "parse expressions with precedence" in {
      parse("12*(34+56)") must_=== Right(
        "" -> Operation(Constant(12), Mul, SubExpr(Operation(Constant(34), Add, Constant(56))))
      )
      parse("2*(3+4*5)") must_=== Right(
        "" -> Operation(
          Constant(2),
          Mul,
          SubExpr(
            Operation(
              Constant(3),
              Add,
              Operation(Constant(4), Mul, Constant(5))
            )
          )
        )
      )
    }
  }

  "Printer" should {
    import Syntax._

    "print a single-digit number" in {
      print(Constant(1)) must_=== Right("1")
    }
    "print a multi-digit number" in {
      print(Constant(23)) must_=== Right("23")
    }

    "print an operation" in {
      print(Operation(Constant(1), Add, Constant(2))) must_=== Right("1+2")
      print(Operation(Constant(1), Mul, Constant(2))) must_=== Right("1*2")
      print(Operation(Operation(Constant(1), Add, Constant(2)), Add, Constant(3))) must_=== Right(
        "1+2+3"
      )
      print(Operation(Constant(1), Add, Operation(Constant(2), Mul, Constant(3)))) must_=== Right(
        "1+2*3"
      )
      print(Operation(Operation(Constant(1), Mul, Constant(2)), Add, Constant(3))) must_=== Right(
        "1*2+3"
      )
      print(
        Operation(
          Operation(Operation(Constant(1), Add, Constant(2)), Add, Constant(3)),
          Add,
          Constant(4)
        )
      ) must_=== Right(
        "1+2+3+4"
      )
      print(
        Operation(
          Operation(Constant(1), Mul, Constant(2)),
          Add,
          Operation(Constant(3), Mul, Constant(4))
        )
      ) must_=== Right(
        "1*2+3*4"
      )
    }

    "not print an incorrectly composed expression" in {
      print(Operation(Constant(1), Add, Operation(Constant(2), Add, Constant(3)))) must_=== Left(
        "Expected: Constant"
      )
      print(
        Operation(
          Operation(Constant(1), Add, Constant(2)),
          Add,
          Operation(Constant(3), Add, Constant(4))
        )
      ) must_=== Left("Expected: Constant")
    }
  }
}
