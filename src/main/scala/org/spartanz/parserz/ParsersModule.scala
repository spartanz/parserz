package org.spartanz.parserz

import scala.annotation.tailrec
import scala.collection.mutable

trait ParsersModule extends ExprModule {
  type Input

  sealed abstract class Grammar[-SI, +SO, +E, A] {
    self =>

    import Grammar._

    final def map[B](to: A => B, from: B => A): Grammar[SI, SO, E, B] =
      Map[SI, SO, E, A, B](self, a => Right(to(a)), b => Right(from(b)))

    final def mapOption[E1 >: E, B](e: E1)(to: A => Option[B], from: B => Option[A]): Grammar[SI, SO, E1, B] =
      Map[SI, SO, E1, A, B](self, asEither(e)(to), asEither(e)(from))

    final def mapPartial[E1 >: E, B](e: E1)(to: A =?> B, from: B =?> A): Grammar[SI, SO, E1, B] =
      Map[SI, SO, E1, A, B](self, asEither(e)(to.lift), asEither(e)(from.lift))

    final def filter[E1 >: E](e: E1)(f: Expr[A]): Grammar[SI, SO, E1, A] =
      Filter[SI, SO, E1, A](self, e, f)

    final def select[SI1 <: SI, SO1 >: SO, E1 >: E, B](f: A => Grammar[SI1, SO1, E1, B])(
      implicit en: Enumerable[A]
    ): Grammar[SI1, SO1, E1, B] =
      Select[SI1, SO1, E1, A, B](self, f, en)

    final def mapStatefully[SI1 <: SI, SO1 >: SO, B](to: (SI1, A) => (SO1, B), from: (SI1, B) => (SO1, A)): Grammar[SI1, SO1, E, B] =
      MapS[SI1, SO1, E, A, B](
        self,
        { case (si, a) => val (so, b) = to(si, a); (so, Right(b)) },
        { case (si, b) => val (so, a) = from(si, b); (so, Right(a)) }
      )

    final def mapStatefullyPartial[SI1 <: SI, SO1 >: SO, E1 >: E, B](
      fe: SI1 => (SO1, E1)
    )(to: (SI1, A) =?> (SO1, B), from: (SI1, B) =?> (SO1, A)): Grammar[SI1, SO1, E1, B] =
      MapS[SI1, SO1, E1, A, B](self, asEither(fe)(to.lift), asEither(fe)(from.lift))

    final def mapOptionS[SI1 <: SI, SO1 >: SO, E1 >: E, B](
      fe: SI1 => (SO1, E1)
    )(to: A => Option[B], from: B => Option[A]): Grammar[SI1, SO1, E1, B] =
      MapES[SI1, SO1, E1, A, B](self, fe, to, from)

    final def mapPartialS[SI1 <: SI, SO1 >: SO, E1 >: E, B](fe: SI1 => (SO1, E1))(to: A =?> B, from: B =?> A): Grammar[SI1, SO1, E1, B] =
      MapES[SI1, SO1, E1, A, B](self, fe, to.lift, from.lift)

    final def filterS[SI1 <: SI, SO1 >: SO, E1 >: E](fe: SI1 => (SO1, E1))(f: Expr[A]): Grammar[SI1, SO1, E1, A] =
      FilterES[SI1, SO1, E1, A](self, fe, f)

    final def zip[SI1 <: SI, SO1 >: SO, E1 >: E, B](that: Grammar[SI1, SO1, E1, B]): Grammar[SI1, SO1, E1, A /\ B] =
      Zip(self, that)

    final def zipL[SI1 <: SI, SO1 >: SO, E1 >: E, B](that: Grammar[SI1, SO1, E1, B], b: B): Grammar[SI1, SO1, E1, A] =
      ZipL(self, that, b)

    final def zipR[SI1 <: SI, SO1 >: SO, E1 >: E, B](a: A, that: Grammar[SI1, SO1, E1, B]): Grammar[SI1, SO1, E1, B] =
      ZipR(self, that, a)

    final def alt[SI1 <: SI, SO1 >: SO, E1 >: E, B](that: Grammar[SI1, SO1, E1, B]): Grammar[SI1, SO1, E1, A \/ B] =
      Alt(self, that)

    final def ∘ [B](to: A => B, from: B => A): Grammar[SI, SO, E, B] = map(to, from)

    final def ~ [SI1 <: SI, SO1 >: SO, E1 >: E, B](that: Grammar[SI1, SO1, E1, B]): Grammar[SI1, SO1, E1, A /\ B] = self.zip(that)

    final def <~ [SI1 <: SI, SO1 >: SO, E1 >: E, B](b: B, that: Grammar[SI1, SO1, E1, B]): Grammar[SI1, SO1, E1, A] = self.zipL(that, b)

    final def | [SI1 <: SI, SO1 >: SO, E1 >: E, B](that: Grammar[SI1, SO1, E1, B]): Grammar[SI1, SO1, E1, A \/ B] = self.alt(that)

    final def rep: Grammar[SI, SO, E, List[A]] = Rep(self)

    final def rep1: Grammar[SI, SO, E, ::[A]] = Rep1(self)

    final def @@ (tag: String): Grammar[SI, SO, E, A] = Tag(self, tag)

    final def tag(tag: String): Grammar[SI, SO, E, A] = self @@ tag
  }

  object Grammar extends GrammarSyntax {
    // format: off
    private[parserz] case class Unit0() extends Grammar[Any, Nothing, Nothing, Unit]
    private[parserz] case class Consume[SI, SO, E, A](to: Input => E \/ (Input, A), from: ((Input, A)) => E \/ Input) extends Grammar[Any, Nothing, E, A]
    private[parserz] case class ConsumeS[SI, SO, E, A](to: (SI, Input) => (SO, E \/ (Input, A)), from: (SI, (Input, A)) => (SO, E \/ Input)) extends Grammar[SI, SO, E, A]
    private[parserz] case class Delay[SI, SO, E, A](delayed: () => Grammar[SI, SO, E, A]) extends Grammar[SI, SO, E, A]
    private[parserz] case class Tag[SI, SO, E, A](value: Grammar[SI, SO, E, A], tag: String) extends Grammar[SI, SO, E, A]
    private[parserz] case class Map[SI, SO, E, A, B](value: Grammar[SI, SO, E, A], to: A => E \/ B, from: B => E \/ A) extends Grammar[SI, SO, E, B]
    private[parserz] case class MapS[SI, SO, E, A, B](value: Grammar[SI, SO, E, A], to: (SI, A) => (SO, E \/ B), from: (SI, B) => (SO, E \/ A)) extends Grammar[SI, SO, E, B]
    private[parserz] case class MapES[SI, SO, E, A, B](value: Grammar[SI, SO, E, A], fe: SI => (SO, E), to: A => Option[B], from: B => Option[A]) extends Grammar[SI, SO, E, B]
    private[parserz] case class Filter[SI, SO, E, A](value: Grammar[SI, SO, E, A], e: E, filter: Expr[A]) extends Grammar[SI, SO, E, A]
    private[parserz] case class FilterES[SI, SO, E, A](value: Grammar[SI, SO, E, A], fe: SI => (SO, E), filter: Expr[A]) extends Grammar[SI, SO, E, A]
    private[parserz] case class Zip[SI, SO, E, A, B](left: Grammar[SI, SO, E, A], right: Grammar[SI, SO, E, B]) extends Grammar[SI, SO, E, A /\ B]
    private[parserz] case class ZipL[SI, SO, E, A, B](left: Grammar[SI, SO, E, A], right: Grammar[SI, SO, E, B], b: B) extends Grammar[SI, SO, E, A]
    private[parserz] case class ZipR[SI, SO, E, A, B](left: Grammar[SI, SO, E, A], right: Grammar[SI, SO, E, B], a: A) extends Grammar[SI, SO, E, B]
    private[parserz] case class Alt[SI, SO, E, A, B](left: Grammar[SI, SO, E, A], right: Grammar[SI, SO, E, B]) extends Grammar[SI, SO, E, A \/ B]
    private[parserz] case class Select[SI, SO, E, A, B](value: Grammar[SI, SO, E, A], f: A => Grammar[SI, SO, E, B], en: Enumerable[A]) extends Grammar[SI, SO, E, B]
    private[parserz] case class Rep[SI, SO, E, A](value: Grammar[SI, SO, E, A]) extends Grammar[SI, SO, E, List[A]]
    private[parserz] case class Rep1[SI, SO, E, A](value: Grammar[SI, SO, E, A]) extends Grammar[SI, SO, E, ::[A]]
    // format: on

    final val unit: Grammar[Any, Nothing, Nothing, Unit] =
      Unit0()

    final def succeed[A](a: A): Grammar[Any, Nothing, Nothing, A] =
      unit.map[A](_ => a, _ => ())

    final def fail[E, A](e: E): Grammar[Any, Nothing, E, A] =
      unit.mapPartial(e)(PartialFunction.empty, PartialFunction.empty)

    final def fail[SI, SO, E, A](fe: SI => (SO, E)): Grammar[SI, SO, E, A] =
      unit.mapPartialS(fe)(PartialFunction.empty, PartialFunction.empty)

    final def consumeStatefully[SI, SO, E, A](
      to: (SI, Input) => (SO, E \/ (Input, A)),
      from: (SI, (Input, A)) => (SO, E \/ Input)
    ): Grammar[SI, SO, E, A] =
      ConsumeS(to, from)

    final def consumeStatefullyOption[SI, SO, E, A](e: E)(
      to: (SI, Input) => (SO, Option[(Input, A)]),
      from: (SI, (Input, A)) => (SO, Option[Input])
    ): Grammar[SI, SO, E, A] =
      consumeStatefully(
        { (si: SI, i: Input) =>
          val (so, r) = to(si, i)
          (so, r.map(Right(_)).getOrElse(Left(e)))
        }, { (si: SI, x: (Input, A)) =>
          val (so, r) = from(si, x)
          (so, r.map(Right(_)).getOrElse(Left(e)))
        }
      )

    final def consume[E, A](to: Input => E \/ (Input, A), from: ((Input, A)) => E \/ Input): Grammar[Any, Nothing, E, A] =
      Consume(to, from)

    final def consumeOption[E, A](
      e: E
    )(to: Input => Option[(Input, A)], from: ((Input, A)) => Option[Input]): Grammar[Any, Nothing, E, A] =
      consume(asEither(e)(to), asEither(e)(from))

    final def delay[SI, SO, E, A](g: => Grammar[SI, SO, E, A]): Grammar[SI, SO, E, A] =
      Delay(() => g)

    private def asEither[E, A, B](e: E)(f: A => Option[B]): A => E \/ B =
      f(_).map(Right(_)).getOrElse(Left(e))

    private def asEither[SI, SO, E, A, B](fe: SI => (SO, E))(f: ((SI, A)) => Option[(SO, B)]): (SI, A) => (SO, E \/ B) =
      (si, a) =>
        f((si, a))
          .map { case (so, b) => so -> Right(b) }
          .getOrElse { val (so, e1) = fe(si); so -> Left(e1) }
  }

  trait GrammarSyntax {

    implicit final class ToGrammarOps1(self: String) {

      def @@ [SI, SO, E, A](g: Grammar[SI, SO, E, A]): Grammar[SI, SO, E, A] = g @@ self
    }

    implicit final class ToZipOps1[SI, SO, E, A, B](self: (Grammar[SI, SO, E, A], A)) {

      def ~> [SI1 <: SI, SO1 >: SO, E1 >: E](that: Grammar[SI1, SO1, E1, B]): Grammar[SI1, SO1, E1, B] =
        self._1.zipR(self._2, that)
    }

    implicit final class ToFoldOps1[SI, SO, E, A, B](self: Grammar[SI, SO, E, (A, List[B])]) {

      def foldLeft(fold: (A, B) => A, unfold: A =?> (A, B)): Grammar[SI, SO, E, A] =
        self.map(
          arg => {
            arg._2.foldLeft(arg._1)(fold)
          },
          arg => {
            @tailrec
            def rec(acc: List[B])(a: A): (A, List[B]) =
              unfold.lift(a) match {
                case None          => (a, acc)
                case Some((a1, b)) => rec(b :: acc)(a1)
              }
            rec(Nil)(arg)
          }
        )
    }
  }

  final def parser[S, E, A](grammar: Grammar[S, S, E, A]): (S, Input) => (S, E \/ (Input, A)) =
    grammar match {
      case Grammar.Unit0()         => (s: S, i: Input) => (s, Right((i, ())))
      case Grammar.Consume(to, _)  => (s: S, i: Input) => (s, to(i))
      case Grammar.ConsumeS(to, _) => (s: S, i: Input) => to(s, i)
      case Grammar.Tag(value, _)   => (s: S, i: Input) => parser(value)(s, i)
      case Grammar.Delay(delayed)  => (s: S, i: Input) => parser(delayed())(s, i)

      case Grammar.Map(value, to, _) =>
        (s: S, i: Input) => {
          val (s1, res1) = parser(value)(s, i)
          (s1, res1.flatMap { case (i1, a) => to(a).map(i1 -> _) })
        }

      case Grammar.MapS(value, to, _) =>
        (s: S, i: Input) => {
          val (s1, res1) = parser(value)(s, i)
          res1.fold(
            e => s1 -> Left(e),
            { case (i1, a) => val (s2, res2) = to(s1, a); (s2, res2.map(i1 -> _)) }
          )
        }

      case Grammar.MapES(value, es, to, _) =>
        (s: S, i: Input) => {
          val (s1, res1) = parser(value)(s, i)
          res1 match {
            case Left(e) =>
              s1 -> Left(e)
            case Right((i1, a)) =>
              to(a)
                .map { b =>
                  s1 -> Right(i1 -> b)
                }
                .getOrElse {
                  val (s2, e) = es(s1)
                  s2 -> Left(e)
                }
          }
        }

      case Grammar.Filter(value, e, expr) =>
        (s: S, i: Input) => {
          val (s1, res1) = parser(value)(s, i)
          s1 -> res1.flatMap {
            case (i1, a) =>
              if (exprFilter(expr)(a)) Right(i1 -> a)
              else Left(e)
          }
        }

      case Grammar.FilterES(value, es, expr) =>
        (s: S, i: Input) => {
          val (s1, res1) = parser(value)(s, i)
          res1 match {
            case Left(e) =>
              s1 -> Left(e)
            case Right((i1, a)) =>
              if (exprFilter(expr)(a))
                s1 -> Right(i1 -> a)
              else {
                val (s2, e) = es(s1)
                s2 -> Left(e)
              }
          }
        }

      case zip: Grammar.Zip[S, S, E, ta, tb] =>
        (s: S, i: Input) => {
          val (s1, res1): (S, E \/ (Input, ta)) = parser(zip.left)(s, i)
          val ret: (S, E \/ (Input, (ta, tb))) = res1 match {
            case Left(e1)       => (s1, Left(e1))
            case Right((i1, a)) =>
              val (s2, res2) = parser(zip.right)(s1, i1)
              (s2, res2.map[(Input, (ta, tb))] { case (i2, b) => (i2, (a, b)) })
          }
          ret
        }

      case zip: Grammar.ZipL[S, S, E, ta, tb] =>
        (s: S, i: Input) => {
          val (s1, res1): (S, E \/ (Input, ta)) = parser(zip.left)(s, i)
          res1 match {
            case Left(e1)       => (s1, Left(e1))
            case Right((i1, a)) =>
              val (s2, res2) = parser(zip.right)(s1, i1)
              (s2, res2.map { case (i2, _) => (i2, a) })
          }
        }

      case zip: Grammar.ZipR[S, S, E, ta, tb] =>
        (s: S, i: Input) => {
          val (s1, res1): (S, E \/ (Input, ta)) = parser(zip.left)(s, i)
          res1 match {
            case Left(e1)       => (s1, Left(e1))
            case Right((i1, _)) => parser(zip.right)(s1, i1)
          }
        }

      case alt: Grammar.Alt[S, S, E, ta, tb] =>
        (s: S, i: Input) => {
          val (s1, res1): (S, E \/ (Input, ta)) = parser(alt.left)(s, i)
          val ret: (S, E \/ (Input, ta \/ tb)) = res1 match {
            case Right((i1, a)) => (s1, Right((i1, Left(a))))
            case Left(_)        =>
              val (s2, res2) = parser(alt.right)(s1, i)
              (s2, res2.map { case (i2, b) => (i2, Right(b)) })
          }
          ret
        }

      case sel: Grammar.Select[S, S, E, _, _] =>
        (s: S, i: Input) => {
          parser(sel.value)(s, i) match {
            case (s1, Left(e))        => (s1, Left(e))
            case (s1, Right((i1, a))) => parser(sel.f(a))(s1, i1)
          }
        }

      case rep: Grammar.Rep[S, S, E, ta] =>
        (s: S, i: Input) => {
          val (s1, i1, as) = repeatParse(rep.value)(s, i, Nil)
          (s1, Right((i1, as.reverse)))
        }

      case rep: Grammar.Rep1[S, S, E, ta] =>
        (s: S, i: Input) => {
          val res1: (S, E \/ (Input, ta)) = parser(rep.value)(s, i)
          val res2: (S, E \/ (Input, ::[ta])) = res1 match {
            case (s1, Left(e)) =>
              (s1, Left(e))
            case (s1, Right((i1, a1))) =>
              val (s2, i2, as) = repeatParse(rep.value)(s1, i1, Nil)
              (s2, Right((i2, ::(a1, as.reverse))))
          }
          res2
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
      case Grammar.Consume(_, from)  => (s: S, a: (Input, A)) => (s, from(a))
      case Grammar.ConsumeS(_, from) => (s: S, a: (Input, A)) => from(s, a)
      case Grammar.Tag(value, _)     => (s: S, a: (Input, A)) => printer(value)(s, a)
      case Grammar.Delay(delayed)    => (s: S, a: (Input, A)) => printer(delayed())(s, a)

      case Grammar.Map(value, _, from) =>
        (s: S, in: (Input, A)) => {
          val (i, a) = in
          from(a).fold(e => s -> Left(e), b => printer(value)(s, (i, b)))
        }

      case Grammar.MapS(value, _, from) =>
        (s: S, in: (Input, A)) => {
          val (i, a)     = in
          val (s1, res1) = from(s, a)
          res1.fold(
            e => s1 -> Left(e),
            b => printer(value)(s1, (i, b))
          )
        }

      case Grammar.MapES(value, es, _, from) =>
        (s: S, in: (Input, A)) => {
          val (i, a) = in
          from(a)
            .map { b =>
              printer(value)(s, (i, b))
            }
            .getOrElse {
              val (s1, e1) = es(s)
              s1 -> Left(e1)
            }
        }

      case Grammar.Filter(value, e, expr) =>
        (s: S, in: (Input, A)) => {
          val (i, a) = in
          if (exprFilter(expr)(a)) printer(value)(s, (i, a))
          else s -> Left(e)
        }

      case Grammar.FilterES(value, es, expr) =>
        (s: S, in: (Input, A)) => {
          val (i, a) = in
          if (exprFilter(expr)(a)) printer(value)(s, (i, a))
          else {
            val (s2, e) = es(s)
            s2 -> Left(e)
          }
        }

      case zip: Grammar.Zip[S, S, E, ta, tb] =>
        (s: S, in: (Input, ta /\ tb)) => {
          val (i, (a, b)) = in
          val (s1, res1)  = printer(zip.left)(s, (i, a))
          res1 match {
            case Left(e)   => (s1, Left(e))
            case Right(i1) => printer(zip.right)(s1, (i1, b))
          }
        }

      case zip: Grammar.ZipL[S, S, E, ta, tb] =>
        (s: S, in: (Input, ta)) => {
          val (i, a)     = in
          val (s1, res1) = printer(zip.left)(s, (i, a))
          res1 match {
            case Left(e)   => (s1, Left(e))
            case Right(i1) => printer(zip.right)(s1, (i1, zip.b))
          }
        }

      case zip: Grammar.ZipR[S, S, E, ta, tb] =>
        (s: S, in: (Input, tb)) => {
          val (i, b)     = in
          val (s1, res1) = printer(zip.left)(s, (i, zip.a))
          res1 match {
            case Left(e)   => (s1, Left(e))
            case Right(i1) => printer(zip.right)(s1, (i1, b))
          }
        }

      case alt: Grammar.Alt[S, S, E, ta, tb] =>
        (s: S, in: (Input, ta \/ tb)) => {
          val (i, ab) = in
          ab match {
            case Left(a)  => printer(alt.left)(s, (i, a))
            case Right(b) => printer(alt.right)(s, (i, b))
          }
        }

      case sel: Grammar.Select[S, S, E, ta, tb] =>
        (s: S, in: (Input, tb)) => {
          val (i, b) = in
          val range = sel.en.range(sel.en.min, sel.en.max)
          val choices = if (range.isEmpty) ::(sel.en.min, Nil) else ::(range.head, range.tail.toList)

          def attempt(s: S, i: Input, a: ta): (S, E \/ Input) =
            printer(sel.value)(s, (i, a)) match {
              case (s1, Left(e1)) => (s1, Left(e1))
              case (s1, Right(i1)) => printer(sel.f(a))(s1, (i1, b))
            }

          choices.tail.foldLeft[(S, E \/ Input)](attempt(s, i, choices.head)) {
            case ((s1, Right(i1)), _) => (s1, Right(i1))
            case ((s1, Left(_)), a)   => attempt(s1, i, a)
          }
        }

      case rep: Grammar.Rep[S, S, E, ta] =>
        (s: S, in: (Input, List[ta])) => {
          val (i, la) = in
          repeatPrint(rep.value)(s, i, la)
        }

      case rep: Grammar.Rep1[S, S, E, ta] =>
        (s: S, in: (Input, List[ta])) => {
          val (i, la) = in
          repeatPrint(rep.value)(s, i, la)
        }
    }

  private def repeatPrint[S, E, A](g: Grammar[S, S, E, A])(s: S, i: Input, as: List[A]): (S, E \/ Input) =
    as.foldLeft[(S, E \/ Input)](s -> Right(i)) {
      case ((s0, Left(e)), _)    => (s0, Left(e))
      case ((s0, Right(i0)), a0) => printer(g)(s0, (i0, a0))
    }

  final def bnf[SI, SO, E, A](grammar: Grammar[SI, SO, E, A]): List[String] = {
    def tagOrExpand[A1](g: Grammar[SI, SO, E, A1]): String =
      g match {
        case Grammar.Unit0()               => ""
        case Grammar.Consume(_, _)         => ""
        case Grammar.ConsumeS(_, _)        => ""
        case Grammar.Delay(delayed)        => tagOrExpand(delayed())
        case Grammar.Tag(_, tag)           => "<" + tag + ">"
        case Grammar.Map(value, _, _)      => tagOrExpand(value)
        case Grammar.MapS(value, _, _)     => tagOrExpand(value)
        case Grammar.MapES(value, _, _, _) => tagOrExpand(value)
        case Grammar.Filter(v, _, expr)    => Some(exprBNF(expr)).filter(_.nonEmpty).getOrElse(tagOrExpand(v))
        case Grammar.FilterES(v, _, expr)  => Some(exprBNF(expr)).filter(_.nonEmpty).getOrElse(tagOrExpand(v))
        case Grammar.Zip(left, right)      => tagOrExpand(left) + " " + tagOrExpand(right)
        case Grammar.ZipL(left, right, _)  => tagOrExpand(left) + " " + tagOrExpand(right)
        case Grammar.ZipR(left, right, _)  => tagOrExpand(left) + " " + tagOrExpand(right)
        case Grammar.Alt(left, right)      => "(" + tagOrExpand(left) + " | " + tagOrExpand(right) + ")"
        case Grammar.Select(_, f, en)      => en.range(en.min, en.max).map(a => tagOrExpand(f(a))).filter(_.nonEmpty).mkString("(", " | ", ")")
        case Grammar.Rep(value)            => "List(" + tagOrExpand(value) + ")"
        case Grammar.Rep1(value)           => "NEL(" + tagOrExpand(value) + ")"
      }

    val visited: mutable.Set[Grammar[SI, SO, E, _]] = mutable.Set.empty

    def show[A1](g: Grammar[SI, SO, E, A1]): List[String -> String] =
      if (visited.contains(g))
        Nil
      else {
        visited += g
        g match {
          case Grammar.Unit0()               => Nil
          case Grammar.Consume(_, _)         => Nil
          case Grammar.ConsumeS(_, _)        => Nil
          case Grammar.Delay(delayed)        => show(delayed())
          case Grammar.Tag(value, tag)       => show(value) ::: List(tag -> tagOrExpand(value))
          case Grammar.Map(value, _, _)      => show(value)
          case Grammar.MapS(value, _, _)     => show(value)
          case Grammar.MapES(value, _, _, _) => show(value)
          case Grammar.Filter(value, _, _)   => show(value)
          case Grammar.FilterES(value, _, _) => show(value)
          case Grammar.Zip(left, right)      => show(left) ::: show(right)
          case Grammar.ZipL(left, right, _)  => show(left) ::: show(right)
          case Grammar.ZipR(left, right, _)  => show(left) ::: show(right)
          case Grammar.Alt(left, right)      => show(left) ::: show(right)
          case Grammar.Select(value, _, _)   => show(value)
          case Grammar.Rep(value)            => show(value)
          case Grammar.Rep1(value)           => show(value)
        }
      }

    show(grammar).collect { case (t, v) if v.nonEmpty => "<" + t + "> ::= " + v }
  }
}
