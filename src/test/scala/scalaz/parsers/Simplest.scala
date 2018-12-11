package scalaz.parsers

import scalaz.data.{ ~>, ∀ }
import scalaz.tc._

object Simplest {

  type /\[A, B] = (A, B)
  type \/[A, B] = Either[A, B]

  type Error      = Unit
  type Result[A]  = Error \/ (List[Char], A)
  type Parser[A]  = List[Char] => Result[A]
  type Printer[A] = A => List[Char]

  object ScalazInstances {
    import scalaz.Scalaz.{ applicativeApply, applyFunctor, monadApplicative }

    implicit val applicativeOption: Applicative[Option] =
      monadApplicative[Option](implicitly)

    implicit val applicativeParser: Applicative[Parser] = instanceOf(
      new ApplicativeClass[Parser] {
        override def pure[A](a: A): Parser[A] =
          chars => Right(chars -> a)
        override def ap[A, B](fa: Parser[A])(fab: Parser[A => B]): Parser[B] =
          fa(_).flatMap { case (cs, a) => fab(cs).map { case (cs2, f) => cs2 -> f(a) } }
        override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
          ap(fa)(pure(f))
      }
    )

    implicit val functorParser: Functor[Parser] =
      applyFunctor[Parser](applicativeApply[Parser](applicativeParser))

    implicit val alternativeParser: Alternative[Parser] = instanceOf(
      new AlternativeClass[Parser] {
        override def or[A](f1: Parser[A], f2: => Parser[A]): Parser[A] =
          chars =>
            (f1(chars), f2(chars)) match {
              case (r1, Left(_)) => r1
              case (Left(_), r2) => r2
              case (r1 @ Right((c1, _)), r2 @ Right((c2, _))) =>
                if (c1.lengthCompare(c2.length) <= 0) r1 else r2
            }
      }
    )

    implicit val OptionToOption: Option ~> Option =
      ∀.mk[Option ~> Option].from(identity)

    implicit val CategoryOfPartialFunctions: Category[PFunction] = instanceOf(
      new CategoryClass[PFunction] {
        override def id[A]: PFunction[A, A] = Option.apply
        override def compose[A, B, C](
          f: PFunction[B, C],
          g: PFunction[A, B]
        ): PFunction[A, C] = g(_).flatMap(f)
      }
    )
  }

  type PFunction[A, B] = A => Option[B]
  type PIso[A, B]      = Iso[Option, Option, A, B]
  object PIso extends ProductIso[Option, Option]

  object Syntax {
    sealed trait Expression
    case class Constant(value: Int)                extends Expression
    case class Sum(e1: Expression, e2: Expression) extends Expression
  }

  object Parser {

    def delay[A](pa: => Parser[A]): Parser[A] =
      pa(_)

    def lift[A](a: A)(implicit F: Applicative[Parser]): Parser[A] =
      F.pure(a)
  }

  object Parsers {
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
      digit ∘ lift(_.toString.toInt, _.toString.head)

    val constant: Parser[Constant] =
      integer ∘ lift(Constant, _.value)

    val constantIso: PIso[Constant, Expression] = unsafe(
      { case a               => a },
      { case n @ Constant(_) => n }
    )

    val sumIso: PIso[Expression /\ (Char /\ Expression), Expression] = unsafe(
      { case (e1, (_, e2)) => Sum(e1, e2) },
      { case Sum(e1, e2)   => e1 -> ('+' -> e2) }
    )

    val case0: Parser[Expression] =
      constant ∘ constantIso

    val case1: Parser[Expression] =
      (case0 /\ (plus /\ case0).many) ∘ foldL(sumIso)

    lazy val expression: Parser[Expression] =
      case1
  }

  object IsoInstances {
    import PIso._
    import ScalazInstances._

    def subset[A](p: A => Boolean): PIso[A, A] = new PIso[A, A] {
      def to: UFV[A, A]   = Some(_).filter(p)
      def from: UGV[A, A] = to
    }

    def id[A]: PIso[A, A] = new PIso[A, A] {
      def to: UFV[A, A]   = Some(_)
      def from: UGV[A, A] = to
    }

    def unsafe[A, B](ab: PartialFunction[A, B], ba: PartialFunction[B, A]): PIso[A, B] =
      liftF(ab.lift, ba.lift)

    def nil[A]: PIso[Unit, List[A]] = unsafe(
      { case ()  => Nil },
      { case Nil => () }
    )

    def nel[A]: PIso[(A, List[A]), List[A]] = unsafe(
      { case (x, xs) => x :: xs },
      { case x :: xs => (x, xs) }
    )

    def list[A]: PIso[Unit \/ (A /\ List[A]), List[A]] = lift(
      {
        case Left(_)        => Nil
        case Right((a, as)) => a :: as
      }, {
        case Nil     => Left(())
        case a :: as => Right((a, as))
      }
    )

    def iterate[A](iso: PIso[A, A]): PIso[A, A] = {
      @annotation.tailrec
      def step(f: A => Option[A], state: A): A =
        f(state) match {
          case Some(state1) => step(f, state1)
          case None         => state
        }
      new PIso[A, A] {
        def to: UFV[A, A]   = a => Some(step(iso.to, a))
        def from: UGV[A, A] = a => Some(step(iso.from, a))
      }
    }

    import PIso._

    def foldL[A, B](iso: PIso[A ⓧ B, A]): PIso[A ⓧ List[B], A] = {
      import Combinators._
      import ScalazInstances._
      def step: PIso[A ⓧ List[B], A ⓧ List[B]] = {
        val first: PIso[A ⓧ List[B], A ⓧ (B ⓧ List[B])] = id[A] ⓧ ~nel[B]
        val app: PIso[A ⓧ B ⓧ List[B], A ⓧ List[B]]     = iso ⓧ id[List[B]]
        first >>> associate >>> app
      }
      iterate(step) >>> (id[A] ⓧ ~nil[B]) >>> ~unitR[A]
    }
  }

  implicit class ParserOps[A](p: Parser[A]) {
    import Parser._
    import IsoInstances._

    def /\ [B](other: Parser[B])(implicit F: Applicative[Parser]): Parser[A /\ B] =
      F.ap(p)(F.ap(other)(F.pure[B => A => A /\ B](b => a => (a, b))))

    def \/ [B](
      other: => Parser[B]
    )(implicit F: Functor[Parser], A: Alternative[Parser]): Parser[A \/ B] =
      A.or(F.map(p)(Left(_)), F.map(other)(Right(_)))

    def || (other: => Parser[A])(implicit A: Alternative[Parser]): Parser[A] =
      A.or(p, other)

    def ∘ [B](iso: PIso[A, B]): Parser[B] =
      p(_).flatMap { case (rest, v) => iso.to(v).fold[Result[B]](Left(()))(b => Right(rest -> b)) }

    def many(implicit F: Applicative[Parser], A: Alternative[Parser]): Parser[List[A]] = {
      import scalaz.Scalaz.{ applicativeApply, applyFunctor }
      lazy val step: Parser[List[A]] = (lift(()) \/ (p /\ delay(step))) ∘ list
      step
    }
  }
}
