package scalaz.parsers

import scalaz.tc.{ Category, CategoryClass, instanceOf }

object Convoluted {

  type Id[A]    = A
  type \/[A, B] = Either[A, B]

  type Error      = Unit
  type Result[A]  = Error \/ (List[Char], A)
  type Parser[A]  = List[Char] => Result[A]
  type Printer[A] = A => List[Char]

  type BaseFunction[F[_], A, B] = A => F[B] // May not need this
  type TFunction[A, B]          = BaseFunction[Id, A, B]
  type PFunction[A, B]          = BaseFunction[Option, A, B]
  type PIso[A, B]               = Iso[Option, Id, A, B]

  implicit def categoryOfTotalFunctions: Category[TFunction] = instanceOf(
    new CategoryClass[TFunction] {
      override def id[A]: TFunction[A, A] = identity
      override def compose[A, B, C](
        f: TFunction[B, C],
        g: TFunction[A, B]
      ): TFunction[A, C] = g.andThen(f)
    }
  )

  implicit def categoryOfPartialFunctions: Category[PFunction] = instanceOf(
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
    case class Number(value: Int) extends Expression
  }

  object Parsers {
    import IsoInstances._
    import Syntax._

    val char: Parser[Char] = {
      case head :: tail => Right(tail -> head)
      case Nil          => Left(())
    }

    val digit: Parser[Char] =
      char ∘ subset(_.isDigit)

    val integer: Parser[Int] =
      digit ∘ apply(_.toString.toInt, _.toString.head)

    val expression: Parser[Expression] =
      integer ∘ (apply[Int, Number](Number, _.value) >>> asExpression)
  }

  object PIso extends IsoClass[Option, Id]

  object IsoInstances {
    import Syntax._
    import PIso._

    def subset[A](p: A => Boolean): PIso[A, A] =
      liftF(Some(_).filter(p), identity)

    def apply[A, B](ab: A => B, ba: B => A): PIso[A, B] =
      liftF(Some(_).map(ab), ba)

    val asExpression: PIso[Number, Expression] =
      liftF(Some(_), { case l @ Number(_) => l })
  }

  implicit class ParserOps[A](p: Parser[A]) {

    def ∘ [B](iso: PIso[A, B]): Parser[B] =
      p(_).flatMap { case (rest, v) => iso.to(v).fold[Result[B]](Left(()))(b => Right(rest -> b)) }
  }
}
