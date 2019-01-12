package scalaz.parsers

import scalaz.data.{ ~>, ∀ }
import scalaz.tc.FoldableClass.{ DeriveFoldMap, DeriveToList }
import scalaz.tc._

object Simplest {

  type Error      = Unit
  type Result[A]  = Error \/ (List[Char], A)
  type Parser[A]  = List[Char] => Result[A]
  type Printer[A] = A => String

  object ScalazInstances {
    import scalaz.Scalaz.monadApplicative

    implicit val productFunctorParser: ProductFunctor[Parser] = instanceOf(
      new ProductFunctorClass[Parser] {
        override def and[A, B](fa: Parser[A], fb: Parser[B]): Parser[A /\ B] =
          fa(_).flatMap { case (cs1, a) => fb(cs1).map { case (cs2, b) => cs2 -> (a -> b) } }
      }
    )

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

    implicit val productFunctorPrinter: ProductFunctor[Printer] = instanceOf(
      new ProductFunctorClass[Printer] {
        override def and[A, B](fa: Printer[A], fb: Printer[B]): Printer[(A, B)] = {
          case (a, b) => fa(a) + fb(b)
        }
      }
    )

    implicit val alternativePrinter: Alternative[Printer] = instanceOf(
      new AlternativeClass[Printer] {
        override def or[A](f1: Printer[A], f2: => Printer[A]): Printer[A] = { a =>
          val s1 = f1(a)
          val s2 = f2(a)
          if (s2.lengthCompare(s1.length) < 0) s1 else s2
        }
      }
    )

    implicit val applicativeOption: Applicative[Option] =
      monadApplicative[Option](implicitly)

    implicit val foldableOption: Foldable[Option] = instanceOf(
      new FoldableClass[Option] with DeriveFoldMap[Option] with DeriveToList[Option] {
        override def foldRight[A, B](fa: Option[A], z: => B)(f: (A, => B) => B): B =
          fa.fold(z)(f(_, z))
        override def foldLeft[A, B](fa: Option[A], z: B)(f: (B, A) => B): B =
          fa.fold(z)(f(z, _))
      }
    )

    implicit val OptionToOption: Option ~> Option =
      ∀.mk[Option ~> Option].from(identity)

    type PFunction[A, B] = A => Option[B]

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

  object Syntax {
    sealed trait Expression
    case class Constant(value: Int)                extends Expression
    case class Sum(e1: Expression, e2: Expression) extends Expression
  }

  def grammar[P[_]: ProductFunctor: Alternative](
    P: ParserSyntax[P, Option, Option]
  ): P[Syntax.Expression] = {
    import Syntax._
    import P._
    import IsoInstances._
    import ScalazInstances._

    type PIso[A, B] = iso.Iso[A, B]

    object IsoInstances {
      import iso._
      import iso.Product._

      def subset[A](p: A => Boolean): PIso[A, A] =
        liftF(Some(_).filter(p), Some(_).filter(p))

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

      def foldL[A, B](iso: PIso[A ⓧ B, A]): PIso[A ⓧ List[B], A] = {
        def step: PIso[A ⓧ List[B], A ⓧ List[B]] = {
          val first: PIso[A ⓧ List[B], A ⓧ (B ⓧ List[B])] = pure[A] ⓧ ~nel[B]
          val app: PIso[A ⓧ B ⓧ List[B], A ⓧ List[B]]     = iso ⓧ pure[List[B]]
          first >>> associate >>> app
        }
        iterate(step) >>> (pure[A] ⓧ ~nil[B]) >>> ~unitR[A]
      }
    }

    val digit: P[Char] =
      char ∘ subset(_.isDigit)

    val plus: P[Char] =
      char ∘ subset(_ == '+')

    val integer: P[Int] =
      digit ∘ iso.lift(_.toString.toInt, _.toString.head)

    val constantIso: PIso[Int, Constant] =
      iso.lift(Constant, _.value)

    val constantExpressionIso: PIso[Constant, Expression] = unsafe(
      { case a               => a },
      { case n @ Constant(_) => n }
    )

    val sumExpressionIso: PIso[Expression /\ (Char /\ Expression), Expression] = unsafe(
      { case (e1, (_, e2)) => Sum(e1, e2) },
      { case Sum(e1, e2)   => e1 -> ('+' -> e2) }
    )

    val constant: P[Constant] =
      integer ∘ constantIso

    val case0: P[Expression] =
      constant ∘ constantExpressionIso

    val case1: P[Expression] =
      (case0 /\ (plus /\ case0).many) ∘ foldL(sumExpressionIso)

    lazy val expression: P[Expression] =
      case1

    expression
  }

  object Parser extends ParserSyntax[Parser, Option, Option] {
    import ScalazInstances._

    override val iso: IsoClass[Option, Option] =
      IsoClass[Option, Option]

    override def char: Parser[Char] = {
      case head :: tail => Right(tail -> head)
      case Nil          => Left(())
    }

    override def lift[A](a: A): Parser[A] =
      chars => Right(chars -> a)

    override def left[A, B](pa: Parser[A]): Parser[A \/ B] =
      pa(_).map { case (s1, a) => s1 -> Left(a) }

    override def right[A, B](pb: Parser[B]): Parser[A \/ B] =
      pb(_).map { case (s1, b) => s1 -> Right(b) }

    override def isoMap[A, B](pa: Parser[A])(instance: iso.Iso[A, B]): Parser[B] =
      pa(_).flatMap {
        case (rest, v) => instance.to(v).fold[Result[B]](Left(()))(b => Right(rest -> b))
      }

    override def delay[A](pa: => Parser[A]): Parser[A] =
      pa(_)
  }

  object Printer extends ParserSyntax[Printer, Option, Option] {
    import ScalazInstances._

    override val iso: IsoClass[Option, Option] =
      IsoClass[Option, Option]

    override def char: Printer[Char] =
      _.toString

    override def lift[A](a: A): Printer[A] =
      _ => ""

    override def left[A, B](pa: Printer[A]): Printer[A \/ B] = {
      case Left(a)  => pa(a)
      case Right(_) => ""
    }

    override def right[A, B](pb: Printer[B]): Printer[A \/ B] = {
      case Left(_)  => ""
      case Right(b) => pb(b)
    }

    override def isoMap[A, B](pa: Printer[A])(instance: iso.Iso[A, B]): Printer[B] =
      instance.from(_).fold("")(pa)

    override def delay[A](pa: => Printer[A]): Printer[A] =
      pa(_)
  }
}
