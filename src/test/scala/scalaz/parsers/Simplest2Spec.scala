package scalaz.parsers

import org.specs2.mutable.Specification
import scalaz.data.{ ~>, ∀ }
import scalaz.tc.FoldableClass.{ DeriveFoldMap, DeriveToList }
import scalaz.tc._

class Simplest2Spec extends Specification {

  object ScalazInstances {
    import scalaz.Scalaz.monadApplicative

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

  object Ex {
    import ScalazInstances._
    import Syntax._
    import EquivInstances._

    val start: TestClass[Option, Option] = TestClass[Option, Option]

    import start.Codec
    import start.Equiv
    import start.Equiv.Product._

    object EquivInstances {

      def subset[A](p: A => Boolean): Equiv[A, A] =
        Equiv.liftF(Some(_).filter(p), Some(_).filter(p))

      def unsafe[A, B](ab: PartialFunction[A, B], ba: PartialFunction[B, A]): Equiv[A, B] =
        Equiv.liftF(ab.lift, ba.lift)

      def nil[A]: Equiv[Unit, List[A]] = unsafe(
        { case ()  => Nil },
        { case Nil => () }
      )

      def nel[A]: Equiv[(A, List[A]), List[A]] = unsafe(
        { case (x, xs) => x :: xs },
        { case x :: xs => (x, xs) }
      )

      def foldL[A, B](equiv: Equiv[A ⓧ B, A]): Equiv[A ⓧ List[B], A] = {
        def step: Equiv[A ⓧ List[B], A ⓧ List[B]] = {
          val first: Equiv[A ⓧ List[B], A ⓧ (B ⓧ List[B])] = nel[B].reverse.second
          val app: Equiv[A ⓧ B ⓧ List[B], A ⓧ List[B]]     = equiv.first
          first >>> associate >>> app
        }
        Equiv.iterate(step) >>> nil[B].second[A].reverse >>> unitR[A].reverse
      }
    }

    val constantIso: Equiv[Int, Constant] =
      Equiv.lift(Constant, _.value)

    val constantExpressionIso: Equiv[Constant, Expression] = unsafe(
      { case a               => a },
      { case n @ Constant(_) => n }
    )

    val sumExpressionIso: Equiv[Expression /\ (Char /\ Expression), Expression] = unsafe(
      { case (e1, (_, e2)) => Sum(e1, e2) },
      { case Sum(e1, e2)   => e1 -> ('+' -> e2) }
    )

    type C[A] = Codec[List[Char], A]

    val char: C[Char] = Codec(
      Equiv.liftF(
        { case h :: t => Some(t -> h); case Nil => None },
        { case (l, c) => Some(c :: l) }
      )
    )

    val digit: C[Char] = char ∘ subset(_.isDigit)

    val plus: C[Char] = char ∘ subset(_ == '+')

    val integer: C[Int] = digit ∘ Equiv.lift(_.toString.toInt, _.toString.head)

    val constant: C[Constant] = integer ∘ constantIso

    val case0: C[Expression] = constant ∘ constantExpressionIso

    val case1: C[Expression] = (case0 ~ (plus ~ case0).many) ∘ foldL(sumExpressionIso)

    lazy val expression: C[Expression] = case1
  }

  def parse(s: String): Option[(List[Char], Syntax.Expression)] =
    Ex.expression.eq.to(s.toCharArray.toList)

  def print(e: Syntax.Expression): Option[String] =
    Ex.expression.eq.from((Nil, e)).map(_.reverse.toArray).map(String.valueOf)

  "Simplest parser" should {
    import Syntax._

    "not parse empty input" in {
      parse("") must_=== None
    }

    "parse a digit into a literal" in {
      parse("5") must_=== Some(Nil -> Constant(5))
    }

    "not parse two digits (because it's indeed simplest)" in {
      parse("55") must_=== Some(List('5') -> Constant(5))
    }

    "not parse a letter and indicate failure" in {
      parse("A") must_=== None
    }

    "not parse '+' by itself" in {
      parse("+") must_=== None
    }

    "parse sum of 2 numbers" in {
      parse("5+6") must_=== Some(Nil -> Sum(Constant(5), Constant(6)))
    }

    "parse sum of 3 numbers" in {
      parse("5+6+7") must_=== Some(Nil -> Sum(Sum(Constant(5), Constant(6)), Constant(7)))
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
