package org.spartanz.parserz

import com.github.ghik.silencer.silent

import scala.annotation.tailrec

trait ParsersModule {
  type Input

  sealed abstract class Grammar[-SI, +SO, +E, A] {
    self =>

    import Grammar._

    final def map[B](to: A => B, from: B => A): Grammar[SI, SO, E, B] =
      Map[SI, SO, E, A, B](self, a => Right(to(a)), b => Right(from(b)))

    final def mapOptional[E1 >: E, B](e: E1)(to: A => Option[B], from: B => Option[A]): Grammar[SI, SO, E1, B] =
      Map[SI, SO, E1, A, B](self, asEither(e)(to), asEither(e)(from))

    final def mapPartial[E1 >: E, B](e: E1)(to: A =?> B, from: B =?> A): Grammar[SI, SO, E1, B] =
      Map[SI, SO, E1, A, B](self, asEither(e)(to.lift), asEither(e)(from.lift))

    final def filter[E1 >: E](e: E1)(f: A => Boolean): Grammar[SI, SO, E1, A] =
      mapPartial[E1, A](e)({ case a if f(a) => a }, { case a if f(a) => a })

    final def zip[SI1 <: SI, SO1 >: SO, E1 >: E, B](that: Grammar[SI1, SO1, E1, B]): Grammar[SI1, SO1, E1, A /\ B] =
      Zip(self, that)

    final def alt[SI1 <: SI, SO1 >: SO, E1 >: E, B](that: Grammar[SI1, SO1, E1, B]): Grammar[SI1, SO1, E1, A \/ B] =
      Alt(self, that)

    final def ∘ [B](to: A => B, from: B => A): Grammar[SI, SO, E, B] = map(to, from)

    final def ~ [SI1 <: SI, SO1 >: SO, E1 >: E, B](that: Grammar[SI1, SO1, E1, B]): Grammar[SI1, SO1, E1, A /\ B] = self.zip(that)

    final def | [SI1 <: SI, SO1 >: SO, E1 >: E, B](that: Grammar[SI1, SO1, E1, B]): Grammar[SI1, SO1, E1, A \/ B] = self.alt(that)

    final def rep: Grammar[SI, SO, E, List[A]] = Rep(self)

    final def rep1: Grammar[SI, SO, E, ::[A]] = Rep1(self)

    final def @@ (tag: String): Grammar[SI, SO, E, A] = Tag(self, tag)

    final def tag(tag: String): Grammar[SI, SO, E, A] = self @@ tag
  }

  object Grammar extends GrammarSyntax {
    // format: off
    private[parserz] case class Unit0() extends Grammar[Any, Nothing, Nothing, Unit]
    private[parserz] case class Consume0[SI, SO, E, A](to: Input => E \/ (Input, A), from: ((Input, A)) => E \/ Input) extends Grammar[Any, Nothing, E, A]
    private[parserz] case class Consume[SI, SO, E, A](to: (SI, Input) => (SO, E \/ (Input, A)), from: (SI, (Input, A)) => (SO, E \/ Input)) extends Grammar[SI, SO, E, A]
    private[parserz] case class Delay[SI, SO, E, A](delayed: () => Grammar[SI, SO, E, A]) extends Grammar[SI, SO, E, A]
    private[parserz] case class Tag[SI, SO, E, A](value: Grammar[SI, SO, E, A], tag: String) extends Grammar[SI, SO, E, A]
    private[parserz] case class Map[SI, SO, E, A, B](value: Grammar[SI, SO, E, A], to: A => E \/ B, from: B => E \/ A) extends Grammar[SI, SO, E, B]

    private[parserz] case class Zip[SI, SO, E, A, B, Z](left: Grammar[SI, SO, E, A], right: Grammar[SI, SO, E, B])(
      @silent implicit val ev: Z =:= (A /\ B)
    ) extends Grammar[SI, SO, E, Z]

    private[parserz] case class Alt[SI, SO, E, A, B, Z](left: Grammar[SI, SO, E, A], right: Grammar[SI, SO, E, B])(
      @silent implicit val ev: Z =:= (A \/ B)
    ) extends Grammar[SI, SO, E, Z]

    private[parserz] case class Rep[SI, SO, E, A, Z](value: Grammar[SI, SO, E, A])(
      @silent implicit val ev: Z =:= List[A]
    ) extends Grammar[SI, SO, E, Z]

    private[parserz] case class Rep1[SI, SO, E, A, Z](value: Grammar[SI, SO, E, A])(
      @silent implicit val ev: Z =:= ::[A]
    ) extends Grammar[SI, SO, E, Z]
    // format: on

    final val unit: Grammar[Any, Nothing, Nothing, Unit] =
      Unit0()

    final def succeed[A](a: A): Grammar[Any, Nothing, Nothing, A] =
      unit.map[A](_ => a, _ => ())

    final def fail[E, A](e: E): Grammar[Any, Nothing, E, A] =
      unit.mapPartial(e)(PartialFunction.empty, PartialFunction.empty)

    final def consume[SI, SO, E, A](
      to: (SI, Input) => (SO, E \/ (Input, A)),
      from: (SI, (Input, A)) => (SO, E \/ Input)
    ): Grammar[SI, SO, E, A] =
      Consume(to, from)

    final def consume0[E, A](to: Input => E \/ (Input, A), from: ((Input, A)) => E \/ Input): Grammar[Any, Nothing, E, A] =
      Consume0(to, from)

    final def consumeOptional0[E, A](
      e: E
    )(to: Input => Option[(Input, A)], from: ((Input, A)) => Option[Input]): Grammar[Any, Nothing, E, A] =
      consume0(asEither(e)(to), asEither(e)(from))

    final def delay[SI, SO, E, A](g: => Grammar[SI, SO, E, A]): Grammar[SI, SO, E, A] =
      Delay(() => g)

    private def asEither[E, A, B](e: E)(f: A => Option[B]): A => E \/ B =
      f(_).map(Right(_)).getOrElse(Left(e))
  }

  trait GrammarSyntax {

    implicit final class ToGrammarOps(self: String) {

      def @@ [SI, SO, E, A](g: Grammar[SI, SO, E, A]): Grammar[SI, SO, E, A] = g @@ self
    }

    implicit final class ToFoldOps1[SI, SO, E, A, B](self: Grammar[SI, SO, E, (A, List[(B, A)])]) {

      def foldLeft(fold: (A, (B, A)) => A, unfold: A =?> (A, (B, A))): Grammar[SI, SO, E, A] =
        self.map(
          arg => arg._2.foldLeft(arg._1)(fold),
          arg => {
            @tailrec
            def rec(acc: List[(B, A)])(a: A): (A, List[(B, A)]) =
              unfold.lift(a) match {
                case None               => (a, acc)
                case Some((a1, (b, a))) => rec((b, a) :: acc)(a1)
              }
            rec(Nil)(arg)
          }
        )
    }
  }

  final def parser[S, E, A](grammar: Grammar[S, S, E, A]): (S, Input) => (S, E \/ (Input, A)) =
    grammar match {
      case Grammar.Unit0()         => (s: S, i: Input) => (s, Right((i, ())))
      case Grammar.Consume0(to, _) => (s: S, i: Input) => (s, to(i))
      case Grammar.Consume(to, _)  => (s: S, i: Input) => to(s, i)
      case Grammar.Tag(value, _)   => (s: S, i: Input) => parser(value)(s, i)
      case Grammar.Delay(delayed)  => (s: S, i: Input) => parser(delayed())(s, i)

      case Grammar.Map(value, to, _) =>
        (s: S, i: Input) => {
          val (s1, res1) = parser(value)(s, i)
          (s1, res1.flatMap { case (i1, a) => to(a).map(i1 -> _) })
        }

      case Grammar.Zip(left, right) =>
        (s: S, i: Input) =>
          parser(left)(s, i) match {
            case (s1, Left(e)) =>
              (s1, Left(e))
            case (s1, Right((i1, a))) =>
              val (s2, res2) = parser(right)(s1, i1)
              (s2, res2.map { case (i2, b) => (i2, (a, b).asInstanceOf[A]) })
          }

      case Grammar.Alt(left, right) =>
        (s: S, i: Input) =>
          parser(left)(s, i) match {
            case (s1, Left(_)) =>
              val (s2, res2) = parser(right)(s1, i)
              (s2, res2.map { case (i2, b) => (i2, Right(b).asInstanceOf[A]) })
            case (s1, Right((i1, a))) =>
              (s1, Right((i1, Left(a).asInstanceOf[A])))
          }

      case Grammar.Rep(value) =>
        (s: S, i: Input) => {
          val (s1, i1, as) = repeatParse(value)(s, i, Nil)
          (s1, Right((i1, as.reverse.asInstanceOf[A])))
        }

      case Grammar.Rep1(value) =>
        (s: S, i: Input) =>
          parser(value)(s, i) match {
            case (s1, Left(e)) =>
              (s1, Left(e))
            case (s1, Right((i1, a1))) =>
              val (s2, i2, as) = repeatParse(value)(s1, i1, Nil)
              (s2, Right((i2, ::(a1, as).asInstanceOf[A])))
          }
    }

  @tailrec
  private def repeatParse[S, E, A](g: Grammar[S, S, E, A])(s: S, i: Input, as: List[A]): (S, Input, List[A]) =
    parser(g)(s, i) match {
      case (s1, Left(_))        => (s1, i, as)
      case (s1, Right((i1, a))) => repeatParse(g)(s1, i1, a :: as)
    }

  final def printer[S, E, A](grammar: Grammar[S, S, E, A]): (S, (Input, A)) => (S, E \/ Input) =
    grammar match {
      case Grammar.Unit0()           => (s: S, a: (Input, A)) => (s, Right(a._1))
      case Grammar.Consume0(_, from) => (s: S, a: (Input, A)) => (s, from(a))
      case Grammar.Consume(_, from)  => (s: S, a: (Input, A)) => from(s, a)
      case Grammar.Tag(value, _)     => (s: S, a: (Input, A)) => printer(value)(s, a)
      case Grammar.Delay(delayed)    => (s: S, a: (Input, A)) => printer(delayed())(s, a)

      case Grammar.Map(value, _, from) =>
        (s: S, a: (Input, A)) => {
          from(a._2).fold(e => s -> Left(e), b => printer(value)(s, (a._1, b)))
        }

      case Grammar.Zip(left, right) =>
        (s: S, a: (Input, A)) => {
          val (i, (a1, a2)) = (a._1, a._2.asInstanceOf[A /\ A])
          val (s1, res1)    = printer(left)(s, (i, a1))
          res1 match {
            case Left(e)   => (s1, Left(e))
            case Right(i1) => printer(right)(s1, (i1, a2))
          }
        }

      case Grammar.Alt(left, right) =>
        (s: S, a: (Input, A)) => {
          val (i, aa) = (a._1, a._2.asInstanceOf[A \/ A])
          aa match {
            case Left(v)  => printer(left)(s, (i, v))
            case Right(v) => printer(right)(s, (i, v))
          }
        }

      case Grammar.Rep(value) =>
        (s: S, a: (Input, A)) => {
          val (i, la) = (a._1, a._2.asInstanceOf[List[A]])
          repeatPrint(value)(s, i, la)
        }

      case Grammar.Rep1(value) =>
        (s: S, a: (Input, A)) => {
          val (i, la) = (a._1, a._2.asInstanceOf[::[A]])
          repeatPrint(value)(s, i, la)
        }
    }

  private def repeatPrint[S, E, A](g: Grammar[S, S, E, A])(s: S, i: Input, as: List[A]): (S, E \/ Input) =
    as.foldLeft[(S, E \/ Input)](s -> Right(i)) {
      case ((s0, Right(i0)), a0) => printer(g)(s0, (i0, a0))
    }

  final def bnf[SI, SO, E, A](grammar: Grammar[SI, SO, E, A]): String = ???
}
