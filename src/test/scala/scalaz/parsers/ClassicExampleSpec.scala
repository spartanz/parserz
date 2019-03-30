package scalaz.parsers

import org.specs2.mutable.Specification
import scalaz.parsers.tc.Category
import scalaz.std.either._
import scalaz.std.string._

class ClassicExampleSpec extends Specification {

  object Syntax {
    sealed trait Expression
    case class Constant(value: Int)                                    extends Expression
    case class Operation(e1: Expression, op: Operator, e2: Expression) extends Expression

    sealed trait Operator
    case object Add extends Operator
    case object Mul extends Operator
  }

  object Example {
    import TCInstances._
    import Syntax._
    import Category._

    val parsing: Parsing[Either[String, ?], Either[String, ?], String] = Parsing()

    import parsing.Equiv
    import parsing.Equiv._

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

    type Codec[A] = parsing.Codec[String, A]

    val char: Codec[Char] = parsing.Codec(
      liftF(
        s =>
          s.headOption
            .fold[Either[String, Char]](Left("Empty input"))(Right(_))
            .map(s.drop(1) -> _),
        { case (s, c) => Right(s + c) }
      )
    )

    val digit: Codec[Char] = char ∘ ensure("Expected: [0-9]")(_.isDigit)

    val plus: Codec[Operator] = char ∘ liftPartial("Expected: '+'")(
      { case '+' => Add },
      { case Add => '+' }
    )

    val star: Codec[Operator] = char ∘ liftPartial("Expected: '*'")(
      { case '*' => Mul },
      { case Mul => '*' }
    )

    val integer: Codec[Int] = digit.many1("Expected at least one digit") ∘ lift(
      chars => new String(chars.toArray).toInt,
      int => int.toString.toList
    )

    val constant: Codec[Constant] = integer ∘ constantEq

    val case0: Codec[Expression] = constant ∘ constantExpressionEq

    val case1: Codec[Expression] = (case0 ~ (star ~ case0).many) ∘ foldl("hm...")(
      operationExpressionEq(Mul)
    )

    val case2: Codec[Expression] = (case1 ~ (plus ~ case1).many) ∘ foldl("hmm..")(
      operationExpressionEq(Add)
    )

    lazy val expression: Codec[Expression] = case2
  }

  def parse(s: String): Either[String, (String, Syntax.Expression)] =
    Example.expression.parse(s)

  def print(e: Syntax.Expression): Either[String, String] =
    Example.expression.print0(e)

  "Example parser" should {
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
  }

  "Example printer" should {
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
