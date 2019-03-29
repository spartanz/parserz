package scalaz.parsers

import org.specs2.mutable.Specification
import scalaz.std.string._
import scalaz.std.list._
import scalaz.{ -\/, EitherT, Monad, MonadError, Writer, \/- }

class ErrorTrackingExampleSpec extends Specification {

  object Syntax {
    sealed trait Expression
    case class Constant(value: Int)                                    extends Expression
    case class Operation(e1: Expression, op: Operator, e2: Expression) extends Expression

    sealed trait Operator
    case object Add extends Operator
    case object Mul extends Operator
  }

  type Eff[A] = EitherT[String, Writer[List[String], ?], A]

  object Parser extends ParserInstances2 {

    val parsing: Parsing[Eff, Eff, String] = Parsing()
    val equiv: parsing.Equiv.type          = parsing.Equiv

    type Equiv[A, B] = parsing.Equiv[A, B]
    type Codec[A]    = parsing.Codec[List[(Char, Int)], A]
  }

  trait ParserInstances2 extends ParserInstances1 {
    implicit val monadError: MonadError[Eff, String] = new MonadError[Eff, String] {
      override def point[A](a: => A): Eff[A]                      = monad.point(a)
      override def bind[A, B](fa: Eff[A])(f: A => Eff[B]): Eff[B] = monad.bind(fa)(f)

      override def raiseError[A](e: String): Eff[A] =
        EitherT(Writer(List(e), -\/(e)))

      override def handleError[A](fa: Eff[A])(f: String => Eff[A]): Eff[A] =
        EitherT(fa.run.flatMap {
          case -\/(e) => f(e).run
          case \/-(_) => fa.run
        })
    }
  }

  trait ParserInstances1 {
    val monad: Monad[Eff] = implicitly
  }

  object Example {
    import Syntax._
    import TCInstances.naturalTransformationLoop

    import Parser._
    import Parser.equiv._

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

    val char: Codec[Char] = parsing.Codec(
      liftF({
        case (h, i) :: t => EitherT(Writer(List("Position: " + i), \/-(t -> h)))
        case Nil         => EitherT.left("Empty input")
      }, {
        case (cc, c) => EitherT.right(cc :+ (c -> 0))
      })
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

    // todo: how to get this error?
    val integer: Codec[Int] = digit.many1("Expected at least one digit") ∘ lift(
      chars => new String(chars.toArray).toInt,
      int => int.toString.toList
    )

    val constant: Codec[Constant] = integer ∘ constantEq

    val case0: Codec[Expression] = constant ∘ constantExpressionEq

    // todo: how to get this error?
    val case1: Codec[Expression] = (case0 ~ (star ~ case0).many) ∘ foldl("hm...")(
      operationExpressionEq(Mul)
    )

    val case2: Codec[Expression] = (case1 ~ (plus ~ case1).many) ∘ foldl("hmm..")(
      operationExpressionEq(Add)
    )

    lazy val expression: Codec[Expression] = case2
  }

  def parse(s: String): (List[String], String \/ (String, Syntax.Expression)) =
    Example.expression
      .parse(
        s.toCharArray.toList.zipWithIndex
      )
      .run
      .map(
        _.toEither.map { case (cc, exp) => cc.unzip._1.mkString -> exp }
      )
      .run

  def parse2(s: String): String \/ (String, Syntax.Expression) =
    parse(s)._2

  def print(e: Syntax.Expression): String \/ String =
    Example.expression.print0(e).run.map(_.toEither).run._2.map(_.unzip._1.mkString)

  "Example parser" should {
    import Syntax._

    "not parse empty input" in {
      parse2("") must_=== Left("Empty input")
    }
    "parse a digit into a literal" in {
      parse2("5") must_=== Right("" -> Constant(5))
    }
    "parse several digits" in {
      parse2("567") must_=== Right("" -> Constant(567))
    }

    "not parse a letter and indicate failure" in {
      parse("A") must_=== List("Position: 0", "Expected: [0-9]") -> Left("Expected: [0-9]")
    }
    "not parse '+' by itself" in {
      parse("+") must_=== List("Position: 0", "Expected: [0-9]") -> Left("Expected: [0-9]")
    }

    "parse operation with 2 numbers" in {
      parse2("5+6") must_=== Right("" -> Operation(Constant(5), Add, Constant(6)))
      parse2("5*6") must_=== Right("" -> Operation(Constant(5), Mul, Constant(6)))
    }
    "parse operation with 3 numbers" in {
      parse2("5+6+7") must_=== Right(
        "" -> Operation(Operation(Constant(5), Add, Constant(6)), Add, Constant(7))
      )
      parse2("5*6*7") must_=== Right(
        "" -> Operation(Operation(Constant(5), Mul, Constant(6)), Mul, Constant(7))
      )
    }

    "report errors" in {
      parse("1a") must_=== List(
        "Position: 0",
        "Position: 1",
        "Expected: [0-9]",
        "Position: 1",
        "Expected: '*'",
        "Position: 1",
        "Expected: '+'"
      ) -> Right("a" -> Constant(1))

      parse("1**2") must_=== List(
        "Position: 0",
        "Position: 1",
        "Expected: [0-9]",
        "Position: 1",
        "Position: 2",
        "Expected: [0-9]",
        "Position: 1",
        "Expected: '+'"
      ) -> Right("**2" -> Constant(1))

      parse("1*2**3") must_=== List(
        "Position: 0",
        "Position: 1",
        "Expected: [0-9]",
        "Position: 1",
        "Position: 2",
        "Position: 3",
        "Expected: [0-9]",
        "Position: 3",
        "Position: 4",
        "Expected: [0-9]",
        "Position: 1",
        "Position: 2",
        "Position: 3",
        "Expected: [0-9]",
        "Position: 3",
        "Position: 4",
        "Expected: [0-9]",
        "Position: 3",
        "Expected: '+'"
      ) -> Right("**3" -> Operation(Constant(1), Mul, Constant(2)))
    }

    "parse expressions" in {
      parse2("12*34+56") must_=== Right(
        "" -> Operation(Operation(Constant(12), Mul, Constant(34)), Add, Constant(56))
      )
      parse2("12+34*56") must_=== Right(
        "" -> Operation(Constant(12), Add, Operation(Constant(34), Mul, Constant(56)))
      )
      parse2("1*2+3*4") must_=== Right(
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
