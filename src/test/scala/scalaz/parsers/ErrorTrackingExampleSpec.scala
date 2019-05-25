package scalaz.parsers

import org.specs2.mutable.Specification
import scalaz.std.anyVal._
import scalaz.std.string._
import scalaz.std.list._
import scalaz.{ -\/, EitherT, Foldable, Monad, MonadError, RWS, ReaderWriterState, \/- }

class ErrorTrackingExampleSpec extends Specification {

  object Syntax {
    sealed trait Expression
    case class Constant(value: Int)                                    extends Expression
    case class Operation(e1: Expression, op: Operator, e2: Expression) extends Expression

    sealed trait Operator
    case object Add extends Operator
    case object Mul extends Operator
  }

  // Effect = Either[Error, Result] + Writer[Position & Error] + State[CurrentPosition]
  type Eff[A] = EitherT[String, RWS[Unit, List[Int /\ String], Int, ?], A]

  object Parser extends ParserInstances2 {

    val parsing: Parsing[Eff, Eff, String] = Parsing()
    val equiv: parsing.Equiv.type          = parsing.Equiv
    val codec: parsing.Codec.type          = parsing.Codec
    val syntax: parsing.syntax.type        = parsing.syntax

    type Equiv[A, B] = parsing.Equiv[A, B]
    type Codec[A]    = parsing.Codec[List[Char /\ Int], A]

    val char: Codec[Char] = codec(
      equiv.liftF({
        case (c, i) :: cs => EitherT(ReaderWriterState((_, _) => (Nil, \/-(cs -> c), i)))
        case Nil          => EitherT.left("Empty input")
      }, {
        case (cs, c) => EitherT.right(cs :+ (c -> 0))
      })
    )
  }

  trait ParserInstances2 extends ParserInstances1 {
    implicit val monadError: MonadError[Eff, String] = new MonadError[Eff, String] {
      override def point[A](a: => A): Eff[A]                      = monad.point(a)
      override def bind[A, B](fa: Eff[A])(f: A => Eff[B]): Eff[B] = monad.bind(fa)(f)

      override def raiseError[A](e: String): Eff[A] =
        EitherT(ReaderWriterState((_, i) => (List(i + 1 -> e), -\/(e), i)))

      override def handleError[A](fa: Eff[A])(f: String => Eff[A]): Eff[A] =
        EitherT(fa.run.flatMap {
          case -\/(e) => f(e).run
          case \/-(_) => fa.run
        })
    }

    type WriterState[A] = RWS[Unit, List[Int /\ String], Int, A]

    implicit val writerStateFoldable: Foldable[WriterState] =
      new Foldable[WriterState] with Foldable.FromFoldr[WriterState] {
        override def foldRight[A, B](fa: WriterState[A], z: => B)(f: (A, => B) => B): B =
          f(fa.runZero(())._2, z)
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
    import Parser.codec._
    import Parser.syntax._

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
    val integer: Codec[Int] = digit
      .many1("Expected at least one digit")
      .emap(ensure("Number is too big")(_.size <= 7))
      .emap(
        lift(
          chars => new String(chars.toArray).toInt,
          int => int.toString.toList
        )
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

    val expression: Codec[Expression] = case2
  }

  def parse(s: String): (List[Int /\ String], String \/ (String, Syntax.Expression)) = {
    val (log, res, _) = Example.expression
      .parse(s.toCharArray.toList.zipWithIndex)
      .run
      .map(_.toEither.map { case (cc, exp) => cc.map(_._1).mkString -> exp })
      .runZero(())
    log -> res
  }

  def parsingResult(s: String): String \/ (String, Syntax.Expression) =
    parse(s)._2

  def lastParsingError(s: String): Option[Int /\ String] =
    Option(parse(s)._1.groupBy(_._1).maxBy(_._1))
      .map { case (p, es) => p -> es.map(_._2).mkString(", ") }

  def print(e: Syntax.Expression): String \/ String =
    Example.expression.print0(e).run.map(_.toEither).runZero(())._2.map(_.map(_._1).mkString)

  "Error tracking parser" should {
    import Syntax._

    "not parse empty input" in {
      parsingResult("") must_=== Left("Empty input")
    }
    "parse a digit into a literal" in {
      parsingResult("5") must_=== Right("" -> Constant(5))
    }
    "not parse number bigger than max" in {
      parsingResult(List.fill(100)('5').mkString) must_=== Left("Number is too big")
    }
    "parse several digits" in {
      parsingResult("567") must_=== Right("" -> Constant(567))
    }

    "not parse a letter and indicate failure" in {
      parsingResult("A") must_=== Left("Expected: [0-9]")
    }
    "not parse '+' by itself" in {
      parsingResult("+") must_=== Left("Expected: [0-9]")
    }

    "parse operation with 2 numbers" in {
      parsingResult("5+6") must_=== Right("" -> Operation(Constant(5), Add, Constant(6)))
      parsingResult("5*6") must_=== Right("" -> Operation(Constant(5), Mul, Constant(6)))
    }
    "parse operation with 3 numbers" in {
      parsingResult("5+6+7") must_=== Right(
        "" -> Operation(Operation(Constant(5), Add, Constant(6)), Add, Constant(7))
      )
      parsingResult("5*6*7") must_=== Right(
        "" -> Operation(Operation(Constant(5), Mul, Constant(6)), Mul, Constant(7))
      )
    }

    "report errors" in {
      lastParsingError("$") must_=== Some(1      -> "Expected: [0-9]")
      lastParsingError("1a") must_=== Some(2     -> "Expected: [0-9], Expected: '*', Expected: '+'")
      lastParsingError("1**2") must_=== Some(3   -> "Expected: [0-9]")
      lastParsingError("1*2**3") must_=== Some(5 -> "Expected: [0-9]")
    }

    "produce detailed log while parsing" in {
      parse("$") must_=== List(
        1 -> "Expected: [0-9]"
      ) -> Left("Expected: [0-9]")

      parse("1a") must_=== List(
        2 -> "Expected: [0-9]",
        2 -> "Expected: '*'",
        2 -> "Expected: '+'"
      ) -> Right("a" -> Constant(1))

      parse("1**2") must_=== List(
        2 -> "Expected: [0-9]",
        3 -> "Expected: [0-9]",
        2 -> "Expected: '+'"
      ) -> Right("**2" -> Constant(1))

      parse("1*2**3") must_=== List(
        2 -> "Expected: [0-9]",
        4 -> "Expected: [0-9]",
        5 -> "Expected: [0-9]",
        4 -> "Expected: '+'"
      ) -> Right("**3" -> Operation(Constant(1), Mul, Constant(2)))
    }

    "parse expressions" in {
      parsingResult("12*34+56") must_=== Right(
        "" -> Operation(Operation(Constant(12), Mul, Constant(34)), Add, Constant(56))
      )
      parsingResult("12+34*56") must_=== Right(
        "" -> Operation(Constant(12), Add, Operation(Constant(34), Mul, Constant(56)))
      )
      parsingResult("1*2+3*4") must_=== Right(
        "" -> Operation(
          Operation(Constant(1), Mul, Constant(2)),
          Add,
          Operation(Constant(3), Mul, Constant(4))
        )
      )
    }
  }

  "Error tracking printer" should {
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
