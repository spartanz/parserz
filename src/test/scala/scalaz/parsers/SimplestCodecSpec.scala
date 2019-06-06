package scalaz.parsers

import org.specs2.mutable.Specification
import scalaz.std.option._
import scalaz.std.string._

class SimplestCodecSpec extends Specification {

  object Syntax {
    sealed trait Expression
    case class Constant(value: Int)                extends Expression
    case class Sum(e1: Expression, e2: Expression) extends Expression
  }

  object Example {
    import Syntax._
    import TCInstances._

    val parsing: Parsing[Option, Option, Unit] = Parsing()

    import parsing.Codec
    import parsing.Equiv
    import parsing.Equiv._

    val constantEq: Equiv[Int, Constant] =
      lift(Constant, _.value)

    val constantExpressionEq: Equiv[Constant, Expression] = liftPartial(())(
      { case a               => a },
      { case n @ Constant(_) => n }
    )

    val sumExpressionEq: Equiv[Expression /\ (Char /\ Expression), Expression] = liftPartial(())(
      { case (e1, (_, e2)) => Sum(e1, e2) },
      { case Sum(e1, e2)   => e1 -> ('+' -> e2) }
    )

    type C[A] = Codec[String, A]

    val char: C[Char] = Codec(
      liftFG(
        s => s.headOption.map(s.drop(1) -> _),
        { case (s, c) => Some(s + c) }
      )
    )

    val digit: C[Char] = char ∘ ensure(())(_.isDigit)

    val plus: C[Char] = char ∘ ensure(())(_ == '+')

    val integer: C[Int] = digit ∘ lift(_.toString.toInt, _.toString.head)

    val constant: C[Constant] = integer ∘ constantEq

    val case0: C[Expression] = constant ∘ constantExpressionEq

    val case1: C[Expression] = (case0 ~ (plus ~ case0).many) ∘ foldl(())(sumExpressionEq)

    lazy val expression: C[Expression] = case1
  }

  def parse(s: String): Option[(String, Syntax.Expression)] =
    Example.expression.parse(s)

  def print(e: Syntax.Expression): Option[String] =
    Example.expression.print0(e)

  "Simplest parser" should {
    import Syntax._

    "not parse empty input" in {
      parse("") must_=== None
    }

    "parse a digit into a literal" in {
      parse("5") must_=== Some("" -> Constant(5))
    }

    "not parse two digits (because it's indeed simplest)" in {
      parse("55") must_=== Some("5" -> Constant(5))
    }

    "not parse a letter and indicate failure" in {
      parse("A") must_=== None
    }

    "not parse '+' by itself" in {
      parse("+") must_=== None
    }

    "parse sum of 2 numbers" in {
      parse("5+6") must_=== Some("" -> Sum(Constant(5), Constant(6)))
    }

    "parse sum of 3 numbers" in {
      parse("5+6+7") must_=== Some("" -> Sum(Sum(Constant(5), Constant(6)), Constant(7)))
    }
  }

  "Simplest printer" should {
    import Syntax._

    "print a constant" in {
      print(Constant(1)) must_=== Some("1")
    }
    "not print a multi-digit number (not yet supported)" in {
      print(Constant(23)) must_=== Some("2")
    }
    "print a sum" in {
      print(Sum(Constant(1), Constant(2))) must_=== Some("1+2")
      print(Sum(Sum(Constant(1), Constant(2)), Constant(3))) must_=== Some("1+2+3")
      print(Sum(Sum(Sum(Constant(1), Constant(2)), Constant(3)), Constant(4))) must_=== Some(
        "1+2+3+4"
      )
    }
    "not print an incorrectly composed expression" in {
      print(Sum(Constant(1), Sum(Constant(2), Constant(3)))) must_=== None
      print(Sum(Sum(Constant(1), Constant(2)), Sum(Constant(3), Constant(4)))) must_=== None
    }
  }
}
