package scalaz.parsers

import scalaz.Scalaz.Id
import scalaz.data.{ ~>, ∀ }
import scalaz.tc._

object Simplest {

  type \/[A, B] = Either[A, B]

  type Error      = Unit
  type Result[A]  = Error \/ (List[Char], A)
  type Parser[A]  = List[Char] => Result[A]
  type Printer[A] = A => List[Char]

  object ScalazInstances {
    import scalaz.Scalaz.{ applicativeApply, applyFunctor, monadApplicative }

    implicit val applicativeId: Applicative[Id] = instanceOf(
      new ApplicativeClass[Id] {
        def pure[A](a: A): Id[A]                      = a
        def ap[A, B](fa: Id[A])(f: Id[A => B]): Id[B] = f(fa)
        def map[A, B](fa: Id[A])(f: A => B): Id[B]    = f(fa)
      }
    )

    implicit val applicativeOption: Applicative[Option] =
      monadApplicative[Option](implicitly)

    implicit val applicativeParser: Applicative[Parser] = instanceOf(
      new ApplicativeClass[Parser] {
        override def pure[A](a: A): Parser[A] =
          chars => Right(chars -> a)
        override def ap[A, B](fa: Parser[A])(fab: Parser[A => B]): Parser[B] =
          chars =>
            fa(chars).flatMap { case (cs, a) => fab(cs).map { case (cs2, f) => cs2 -> f(a) } }
        override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
          ap(fa)(pure(f))
      }
    )

    implicit val functorParser: Functor[Parser] =
      applyFunctor[Parser](applicativeApply[Parser](applicativeParser))

    implicit val alternativeParser: Alternative[Parser] = instanceOf(
      new AlternativeClass[Parser] {
        override def or[A](f1: Parser[A], f2: => Parser[A]): Parser[A] =
          chars => f1(chars).fold(_ => f2(chars), Right(_))
      }
    )

    implicit val ntIdOption: Id ~> Option = ∀.mk[Id ~> Option].from(Some.apply)
    implicit val ntOptionId: Option ~> Id = ∀.mk[Option ~> Id].from(_.get)
  }

  type TFunction[A, B] = A => Id[B]
  type PFunction[A, B] = A => Option[B]
  type PIso[A, B]      = Iso[Option, Id, A, B]

  object PIso extends ProductIso[Option, Id]

  implicit val categoryOfTotalFunctions: Category[TFunction] = instanceOf(
    new CategoryClass[TFunction] {
      override def id[A]: TFunction[A, A] = identity
      override def compose[A, B, C](
        f: TFunction[B, C],
        g: TFunction[A, B]
      ): TFunction[A, C] = g.andThen(f)
    }
  )

  implicit val categoryOfPartialFunctions: Category[PFunction] = instanceOf(
    new CategoryClass[PFunction] {
      override def id[A]: PFunction[A, A] = Option.apply
      override def compose[A, B, C](
        f: PFunction[B, C],
        g: PFunction[A, B]
      ): PFunction[A, C] = g(_).flatMap(f)
    }
  )

  object Syntax {
    sealed trait Expression
    case class Number(value: Int)                  extends Expression
    case class Composition(n1: Number, n2: Number) extends Expression
  }

  object Parsers {
    import Combinators._
    import PIso._
    import IsoInstances._
    import ScalazInstances._
    import Syntax._

    val char: Parser[Char] = {
      case head :: tail => Right(tail -> head)
      case Nil          => Left(())
    }

    val digit: Parser[Char] =
      char ∘ subset(_.isDigit)

    val plus: Parser[Char] =
      char ∘ subset(_ == '+')

    val integer: Parser[Int] =
      digit ∘ apply(_.toString.toInt, _.toString.head)

    val number: Parser[Number] =
      integer ∘ apply(Number, _.value)

    val composition: Parser[Composition] =
      (number /\ plus /\ number) ∘ (~associate[Number, Char, Number] >>> flatten >>> apply(
        { case (n1, _, n2) => Composition(n1, n2) },
        c => (c.n1, '+', c.n2)
      ))

    val expression: Parser[Expression] =
      (composition \/ number) ∘ toExpression
  }

  object IsoInstances {
    import Syntax._

    def subset[A](p: A => Boolean): PIso[A, A] = new PIso[A, A] {
      def to: UFV[A, A]   = Some(_).filter(p)
      def from: UGV[A, A] = identity
    }

    def id[A]: PIso[A, A] = new PIso[A, A] {
      override def to: UFV[A, A]   = Some(_)
      override def from: UGV[A, A] = identity
    }

    def apply[A, B](ab: A => B, ba: B => A): PIso[A, B] = new PIso[A, B] {
      override def to: UFV[A, B]   = a => Some(a).map(ab)
      override def from: UGV[B, A] = ba
    }

    val toExpression: PIso[Composition \/ Number, Expression] =
      new PIso[Composition \/ Number, Expression] {
        override def to: UFV[Composition \/ Number, Expression] = _.fold(Some(_), Some(_))
        override def from: UGV[Expression, Composition \/ Number] = {
          case c @ Composition(_, _) => Left(c)
          case n @ Number(_)         => Right(n)
        }
      }
  }

  implicit class ParserOps[A](p: Parser[A]) {

    def /\ [B](other: Parser[B])(implicit F: Applicative[Parser]): Parser[(A, B)] =
      F.ap(other)(F.ap(p)(F.pure[A => B => (A, B)](a => b => (a, b))))

    def \/ [B](
      other: => Parser[B]
    )(implicit F: Functor[Parser], A: Alternative[Parser]): Parser[A \/ B] =
      A.or(F.map(p)(Left(_)), F.map(other)(Right(_)))

    def ∘ [B](iso: PIso[A, B]): Parser[B] =
      p(_).flatMap { case (rest, v) => iso.to(v).fold[Result[B]](Left(()))(b => Right(rest -> b)) }
  }
}
