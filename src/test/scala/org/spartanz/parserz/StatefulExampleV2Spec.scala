package org.spartanz.parserz

import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class StatefulExampleV2Spec extends Specification {

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

    import Parser.Grammar._
    import Parser._
    import Syntax._

    case class State(size: Int, cur: Int, err: List[String]) {
      def acc(e: String): State = copy(err = e :: err)
    }

    object State {
      def acc(e: String): State => (State, String) = _.acc(e) -> e
    }

    type S    = State
    type E    = String
    type G[A] = Grammar[S, S, E, A]

    val char: G[Char] = "char" @@ consumeStatefullyOption("expected: char")(
      {
        case (s, i) =>
          i.headOption.map(i.drop(1) -> _) match {
            case x @ Some(_) =>
              val cur = s.size - (i.length - 1)
              (State(s.size, cur, if (s.cur == cur) s.err else Nil), x)
            case None =>
              (s, None)
          }
      }, {
        case (s, (i, c)) => (s, Some(i + c.toString))
      }
    )

    val eof: G[Unit] = consumeStatefully(
      { case (s, i)      => if (i.isEmpty) (s, Right((i, ()))) else (s.acc("expected: eof"), Left("expected: eof")) },
      { case (s, (i, _)) => (s, Right(i)) }
    )

    val digit: G[Char]  = char.filter(State.acc("expected: digit"))(_.isDigit).tag("digit")
    val paren1: G[Char] = char.filter(State.acc("expected: open paren"))(_ == '(').tag("(")
    val paren2: G[Char] = char.filter(State.acc("expected: close paren"))(_ == ')').tag(")")

    val plus: G[Operator] = "+" @@ char.mapPartial(State.acc("expected: '+'"))(
      { case '+' => Add },
      { case Add => '+' }
    )

    val star: G[Operator] = "*" @@ char.mapOption(State.acc("expected: '*'"))(
      { case '*' => Some(Mul); case _ => None },
      { case Mul => Some('*'); case _ => None }
    )

    val integer: G[Int] = "integer" @@ digit.rep1
      .mapStatefullyPartial(State.acc("Number is too big"))(
        { case (s, chars) if chars.size <= 7          => (s, chars.mkString.toInt) },
        { case (s, int) if 0 <= int && int <= 9999999 => val chars = int.toString.toList; (s, ::(chars.head, chars.tail)) }
      )

    val constant: G[Expression] = "Constant" @@ integer.mapPartial(State.acc("expected: Constant"))(
      { case i           => Constant(i) },
      { case Constant(i) => i }
    )

    val multiplier: G[Expression] = "Multiplier" @@ ((paren1 ~ addition ~ paren2) | constant).mapStatefully({
      case (s, Left(((_, exp), _))) => (s, SubExpr(exp))
      case (s, Right(exp))          => (s, exp)
    }, {
      case (s, SubExpr(exp)) => (s, Left((('(', exp), ')')))
      case (s, exp)          => (s, Right(exp))
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

    val expr: G[Expression] = (addition ~ eof).map(_._1, (_, ()))

    val parser: (S, Input) => (S, E \/ (Input, Expression))  = Parser.parser[S, E, Expression](expr)
    val printer: (S, (Input, Expression)) => (S, E \/ Input) = Parser.printer[S, E, Expression](expr)
  }

  import Example.State
  import Syntax._

  private def parse(s: String)  = { val (st, r) = Example.parser(State(s.length, 0, Nil), s); (st.cur, st.err.reverse, r) }
  private def parse0(s: String) = parse(s)._3.right.get._2

  private def print(e: Expression)  = { val (st, r) = Example.printer(State(0, 0, Nil), ("", e)); (st.cur, st.err.reverse, r) }
  private def print0(e: Expression) = print(e)._3.right.get

  private def loop0(s: String, e: Expression): MatchResult[Any] = {
    val parsed  = parse0(s)
    val printed = print0(parsed)
    (parsed must_=== e).and(printed must_=== s)
  }

  "empty" in {
    parse("") must_=== ((0, Nil, Left("expected: char")))
  }
  "single letter" in {
    parse("A") must_=== ((1, List("expected: digit"), Left("expected: digit")))
  }
  "just the plus" in {
    parse("+") must_=== ((1, List("expected: digit"), Left("expected: digit")))
  }
  "hex number" in {
    parse("1a") must_=== ((2, List("expected: digit", "expected: '*'", "expected: '+'", "expected: eof"), Left("expected: eof")))
  }
  "single digit" in {
    loop0("1", Constant(1))
  }
  "several digits" in {
    loop0("12", Constant(12))
  }
  "mul of two" in {
    loop0("1*2", Operation(Constant(1), Mul, Constant(2)))
  }
  "sum of three" in {
    loop0("1+2+3", Operation(Operation(Constant(1), Add, Constant(2)), Add, Constant(3)))
  }
  "more than enough digits" in {
    parse(List.fill(100)('5').mkString) must_=== ((100, List("Number is too big"), Left("Number is too big")))
  }
  "mul of two starred" in {
    parse("1*2*****") must_=== ((4, List("expected: '+'", "expected: eof"), Left("expected: eof")))
  }
  "sum of three plussed" in {
    parse("1+2+3+++++") must_=== ((7, List("expected: digit", "expected: eof"), Left("expected: eof")))
  }
  "incorrect number" in {
    print(Constant(12345678)) must_=== ((0, List("Number is too big"), Left("Number is too big")))
  }
  "incorrect composition" in {
    print(SubExpr(Constant(1))) must_=== ((0, List("expected: Constant"), Left("expected: Constant")))
  }
  "docs" in {
    Example.Parser.bnf(Example.expr).mkString("\n", "\n", "\n") must_===
      """
        |<digit> ::= <char>
        |<integer> ::= NEL(<digit>)
        |<Constant> ::= <integer>
        |<*> ::= <char>
        |<(> ::= <char>
        |<)> ::= <char>
        |<Multiplier> ::= (<(> <Addition> <)> | <Constant>)
        |<Multiplication> ::= <Constant> List(<*> <Multiplier>)
        |<+> ::= <char>
        |<Addition> ::= <Multiplication> List(<+> <Multiplication>)
        |""".stripMargin
  }
}
