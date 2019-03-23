package scalaz.parsers

import scalaz.Zip
import scalaz.parsers.syntax.ParserSyntax
import scalaz.parsers.tc.Alternative
import scalaz.std.option._

object Simplest {

  type Error      = Unit
  type Result[A]  = Error \/ (List[Char], A)
  type Parser[A]  = List[Char] => Result[A]
  type Printer[A] = A => String

  object ScalazInstances {

    implicit val parserZip: Zip[Parser] =
      new Zip[Parser] {
        override def zip[A, B](fa: => Parser[A], fb: => Parser[B]): Parser[A /\ B] =
          fa(_).flatMap { case (cs1, a) => fb(cs1).map { case (cs2, b) => cs2 -> (a -> b) } }
      }

    implicit val parserAlternative: Alternative[Parser] =
      new Alternative[Parser] {
        override def or[A](f1: Parser[A], f2: => Parser[A]): Parser[A] =
          chars =>
            (f1(chars), f2(chars)) match {
              case (r1, Left(_)) => r1
              case (Left(_), r2) => r2
              case (r1 @ Right((c1, _)), r2 @ Right((c2, _))) =>
                if (c1.lengthCompare(c2.length) <= 0) r1 else r2
            }
      }

    implicit val printerZip: Zip[Printer] =
      new Zip[Printer] {
        override def zip[A, B](fa: => Printer[A], fb: => Printer[B]): Printer[(A, B)] = {
          case (a, b) => fa(a) + fb(b)
        }
      }

    implicit val printerAlternative: Alternative[Printer] =
      new Alternative[Printer] {
        override def or[A](f1: Printer[A], f2: => Printer[A]): Printer[A] = { a =>
          val s1 = f1(a)
          val s2 = f2(a)
          if (s2.lengthCompare(s1.length) < 0) s1 else s2
        }
      }
  }

  object Syntax {
    sealed trait Expression
    case class Constant(value: Int)                extends Expression
    case class Sum(e1: Expression, e2: Expression) extends Expression
  }

  def grammar[P[_]: Zip: Alternative](
    P: ParserSyntax[P, Option, Option, Unit]
  ): P[Syntax.Expression] = {
    import Syntax._
    import P._
    import parsing.Equiv._
    import TCInstances._

    type Equiv[A, B] = parsing.Equiv[A, B]

    val digit: P[Char] =
      char ∘ ensure(())(_.isDigit)

    val plus: P[Char] =
      char ∘ ensure(())(_ == '+')

    val integer: P[Int] =
      digit ∘ lift(_.toString.toInt, _.toString.head)

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

    val constant: P[Constant] =
      integer ∘ constantEq

    val case0: P[Expression] =
      constant ∘ constantExpressionEq

    val case1: P[Expression] =
      (case0 /\ (plus /\ case0).many) ∘ foldl(())(sumExpressionEq)

    lazy val expression: P[Expression] =
      case1

    expression
  }

  object Parser extends ParserSyntax[Parser, Option, Option, Unit] {
    import TCInstances._

    override val parsing: Parsing[Option, Option, Unit] =
      Parsing[Option, Option, Unit]

    override def char: Parser[Char] = {
      case head :: tail => Right(tail -> head)
      case Nil          => Left(())
    }

    override def pure[A](a: A): Parser[A] =
      chars => Right(chars -> a)

    override def left[A, B](pa: Parser[A]): Parser[A \/ B] =
      pa(_).map { case (s1, a) => s1 -> Left(a) }

    override def right[A, B](pb: Parser[B]): Parser[A \/ B] =
      pb(_).map { case (s1, b) => s1 -> Right(b) }

    override def imap[A, B](pa: Parser[A])(instance: parsing.Equiv[A, B]): Parser[B] =
      pa(_).flatMap {
        case (rest, v) => instance.to(v).fold[Result[B]](Left(()))(b => Right(rest -> b))
      }

    override def delay[A](pa: => Parser[A]): Parser[A] =
      pa(_)
  }

  object Printer extends ParserSyntax[Printer, Option, Option, Unit] {
    import TCInstances._

    override val parsing: Parsing[Option, Option, Unit] =
      Parsing[Option, Option, Unit]

    override def char: Printer[Char] =
      _.toString

    override def pure[A](a: A): Printer[A] =
      _ => ""

    override def left[A, B](pa: Printer[A]): Printer[A \/ B] = {
      case Left(a)  => pa(a)
      case Right(_) => ""
    }

    override def right[A, B](pb: Printer[B]): Printer[A \/ B] = {
      case Left(_)  => ""
      case Right(b) => pb(b)
    }

    override def imap[A, B](pa: Printer[A])(instance: parsing.Equiv[A, B]): Printer[B] =
      instance.from(_).fold("")(pa)

    override def delay[A](pa: => Printer[A]): Printer[A] =
      pa(_)
  }
}
