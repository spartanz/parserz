package org.spartanz.parserz

import scala.annotation.tailrec
import scala.reflect.ClassTag

trait ParsersModule2 {
  type Input

  import Expr._

  sealed abstract class Grammar[-SI, +SO, +E, A] {
    self =>

    import Grammar._
    import Grammar.GADT._

    final def map[B](to: A => B, from: B => A): Grammar[SI, SO, E, B] =
      Map[SI, SO, E, A, B](self, a => Right(to(a)), b => Right(from(b)))

    final def mapOption[E1 >: E, B](e: E1)(to: A => Option[B], from: B => Option[A]): Grammar[SI, SO, E1, B] =
      Map[SI, SO, E1, A, B](self, asEither(e)(to), asEither(e)(from))

    final def mapEither[E1 >: E, B](to: A => E1 \/ B, from: B => E1 \/ A): Grammar[SI, SO, E1, B] =
      Map[SI, SO, E1, A, B](self, to, from)

    final def mapPartial[E1 >: E, B](e: E1)(to: A =?> B, from: B =?> A): Grammar[SI, SO, E1, B] =
      Map[SI, SO, E1, A, B](self, asEither(e)(to.lift), asEither(e)(from.lift))

    final def filter[E1 >: E](e: E1)(f: Expr[A]): Grammar[SI, SO, E1, A] =
      Filter[SI, SO, E1, A](self, e, f)

    final def option: Grammar[SI, SO, E, Option[A]] =
      alt(succeed(None)).map({
        case Left(v)  => Some(v)
        case Right(_) => None
      }, {
        case Some(v) => Left(v)
        case None    => Right(None)
      })

    final def recover(default: A): Grammar[SI, SO, E, A] =
      alt(succeed(default)).map(_.merge, Left(_))

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

    final def separated[SI1 <: SI, SO1 >: SO, E1 >: E, S](by: Grammar[SI1, SO1, E1, S]): Grammar[SI1, SO1, E1, SeparatedBy[A, S]] = Sep(self, by)

    final def @@ (tag: String): Grammar[SI, SO, E, A] = Tag(self, tag)

    final def tag(tag: String): Grammar[SI, SO, E, A] = self @@ tag
  }

  object Grammar extends GrammarSyntax {
    // format: off
    object GADT {
      private[parserz] case class Produce[SI, SO, E, A](a: A) extends Grammar[Any, Nothing, Nothing, A]
      private[parserz] case class Consume[SI, SO, E, A](consumer: InOut.Consumer.Simple[E, Input, A], condition: A => Boolean, e: E) extends Grammar[SI, SO, E, A]
      private[parserz] case class ConsumeMany[SI, SO, E, A](consumer: InOut.Consumer.Mutable[E, Input, A], e: E) extends Grammar[SI, SO, E, A]
      private[parserz] case class ConsumeToken[SI, SO, E, A](consumer: InOut.Consumer.Mutable[E, Input, A], e: E) extends Grammar[SI, SO, E, A]
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
      private[parserz] case class Sep[SI, SO, E, A, S](value: Grammar[SI, SO, E, A], sep: Grammar[SI, SO, E, S]) extends Grammar[SI, SO, E, SeparatedBy[A, S]]
    }
    // format: on

    final val unit: Grammar[Any, Nothing, Nothing, scala.Unit] =
      GADT.Produce(())

    final def succeed[A](a: A): Grammar[Any, Nothing, Nothing, A] =
      GADT.Produce(a)

    final def fail[E, A](e: E): Grammar[Any, Nothing, E, A] =
      unit.mapPartial(e)(PartialFunction.empty, PartialFunction.empty)

    final def fail[SI, SO, E, A](fe: SI => (SO, E)): Grammar[SI, SO, E, A] =
      unit.mapPartialS(fe)(PartialFunction.empty, PartialFunction.empty)

    final def consume[E, A](e: E, codec: InOut.One[E, Input, A]): Grammar[Any, Nothing, E, A] =
      GADT.Consume(codec.consumer, codec.condition, e)

    final def consume[E, A](e: E, codec: InOut.Many[E, Input, A]): Grammar[Any, Nothing, E, A] =
      GADT.ConsumeMany(codec.consumer, e)

    final def consumeToken[E, A](e: E, codec: InOut.ManyConditional[E, Input, A]): Grammar[Any, Nothing, E, A] =
      GADT.ConsumeToken(codec.consumer, e)

    final def delay[SI, SO, E, A](g: => Grammar[SI, SO, E, A]): Grammar[SI, SO, E, A] =
      GADT.Delay(() => g)

    private def asEither[E, A, B](e: E)(f: A => Option[B]): A => E \/ B =
      f(_).map(Right(_)).getOrElse(Left(e))

    private def asEither[SI, SO, E, A, B](fe: SI => (SO, E))(f: ((SI, A)) => Option[(SO, B)]): (SI, A) => (SO, E \/ B) =
      (si, a) =>
        f((si, a))
          .map { case (so, b) => so -> Right(b) }
          .getOrElse { val (so, e1) = fe(si); so -> Left(e1) }
  }

  trait GrammarSyntax {

    implicit final class ToStringOps1(self: String) {

      def @@ [SI, SO, E, A](g: Grammar[SI, SO, E, A]): Grammar[SI, SO, E, A] =
        g @@ self
    }

    implicit final class ToZipOps1[SI, SO, E, A, B](self: (Grammar[SI, SO, E, A], A)) {

      def ~> [SI1 <: SI, SO1 >: SO, E1 >: E](that: Grammar[SI1, SO1, E1, B]): Grammar[SI1, SO1, E1, B] =
        self._1.zipR(self._2, that)
    }

    implicit final class ToGrammarOps1[SI, SO, E, A, B](self: Grammar[SI, SO, E, List[A]]) {

      def orEmpty: Grammar[SI, SO, E, List[A]] =
        self.recover(Nil)
    }

    implicit final class ToGrammarOps2[SI, SO, E, A: ClassTag, B](self: Grammar[SI, SO, E, Array[A]]) {

      def orEmpty: Grammar[SI, SO, E, Array[A]] =
        self.recover(Array.empty[A])
    }

    implicit final class ToGrammarOps3[SI, SO, E, A, B](self: Grammar[SI, SO, E, (A, List[B])]) {

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



  private class ParserState[+E](val input: Input, var i: Int, var e: List[InOut.Error])

  final def parser[S, E, A](grammar: Grammar[S, S, E, A]): (S, Input) => (S, E \/ A) = {
    (s: S, input: Input) => {
      step(grammar, new ParserState(input, 0, Nil), s)
    }
  }

  private def step[S, E, A](grammar: Grammar[S, S, E, A], ps: ParserState[E], s: S): (S, E \/ A) =
    grammar match {
      case Grammar.GADT.Consume(c, cond, e) => (s, {
        import InOut.Consumer.Simple._
        val cas = c.feed(c.create(), ps.input, ps.i)
        ps.i += extractCount(cas)
        try {
          val res = c.finish(extractState(cas))
          if (cond == null || cond(res)) Right(res) else Left(e)
        }
        catch {
          case InOut.NoInput  => Left(e)
        }
      })

      case Grammar.GADT.ConsumeMany(c, e) => (s, {
        val state = c.create()
        val count = c.feed(state, ps.input, ps.i)
        ps.i += count
        try {
          Right(c.finish(state))
        }
        catch {
          case InOut.NoInput  => Left(e)
        }
      })

      case Grammar.GADT.ConsumeToken(c, e) => (s, {
        val state = c.create()
        val count = c.feed(state, ps.input, ps.i)
        ps.i += count
        try {
          val res = c.finish(state)
          if (res == null) Left(e) else Right(res)
        }
        catch {
          case InOut.NotFound => Left(e)
          case InOut.NoInput  => Left(e)
        }
      })


      case Grammar.GADT.Produce(a)      => (s, Right(a))
      case Grammar.GADT.Tag(value, _)   => step(value, ps, s)
      case Grammar.GADT.Delay(delayed)  => step(delayed(), ps, s)

      case Grammar.GADT.Map(value, to, _) =>
        val (s1, res1) = step(value, ps, s)
        (s1, res1.flatMap(to))

      case Grammar.GADT.MapS(value, to, _) =>
        val (s1, res1) = step(value, ps, s)
        res1.fold(
          e => s1 -> Left(e),
          a => to(s1, a)
        )

      case Grammar.GADT.MapES(value, es, to, _) =>
        val (s1, res1) = step(value, ps, s)
        res1 match {
          case Left(e) =>
            s1 -> Left(e)
          case Right(a) =>
            to(a)
              .map { b =>
                s1 -> Right(b)
              }
              .getOrElse {
                val (s2, e) = es(s1)
                s2 -> Left(e)
              }
        }

      case Grammar.GADT.Filter(value, e, expr) =>
        val (s1, res1) = step(value, ps, s)
        s1 -> res1.flatMap {
          a =>
            if (exprFilter(expr)(a)) Right(a)
            else Left(e)
        }

      case Grammar.GADT.FilterES(value, es, expr) =>
        val (s1, res1) = step(value, ps, s)
        res1 match {
          case Left(e) =>
            s1 -> Left(e)
          case Right(a) =>
            if (exprFilter(expr)(a))
              s1 -> Right(a)
            else {
              val (s2, e) = es(s1)
              s2 -> Left(e)
            }
        }

      case zip: Grammar.GADT.Zip[S, S, E, ta, tb] =>
        val (s1, res1): (S, E \/ ta) = step(zip.left, ps, s)
        val ret: (S, E \/ (ta, tb)) = res1 match {
          case Left(e1) => (s1, Left(e1))
          case Right(a) =>
            val (s2, res2) = step(zip.right, ps, s1)
            (s2, res2.map[(ta, tb)](b => (a, b)))
        }
        ret

      case zip: Grammar.GADT.ZipL[S, S, E, ta, tb] =>
        val (s1, res1): (S, E \/ ta) = step(zip.left, ps, s)
        res1 match {
          case Left(e1) => (s1, Left(e1))
          case Right(a) =>
            val (s2, res2) = step(zip.right, ps, s1)
            (s2, res2.map(_ => a))
        }

      case zip: Grammar.GADT.ZipR[S, S, E, ta, tb] =>
        val (s1, res1): (S, E \/ ta) = step(zip.left, ps, s)
        res1 match {
          case Left(e1) => (s1, Left(e1))
          case Right(_) => step(zip.right, ps, s1)
        }

      case alt: Grammar.GADT.Alt[S, S, E, ta, tb] =>
        val checkpoint = ps.i
        val (s1, res1): (S, E \/ ta) = step(alt.left, ps, s)
        val ret: (S, E \/ (ta \/ tb)) = res1 match {
          case Right(a) => (s1, Right(Left(a)))
          case Left(_)  =>
            ps.i = checkpoint
            val (s2, res2) = step(alt.right, ps, s1)
            (s2, res2.map(Right(_)))
        }
        ret

      case sel: Grammar.GADT.Select[S, S, E, _, _] =>
        step(sel.value, ps, s) match {
          case (s1, Left(e))  => (s1, Left(e))
          case (s1, Right(a)) => step(sel.f(a), ps, s1)
        }

      case rep: Grammar.GADT.Rep[S, S, E, ta] =>
        val (s1, as) = repeatStep(rep.value, ps, s, Nil)
        (s1, Right(as.reverse))

      case rep: Grammar.GADT.Rep1[S, S, E, ta] =>
        val res1: (S, E \/ ta) = step(rep.value, ps, s)
        val res2: (S, E \/ ::[ta]) = res1 match {
          case (s1, Left(e)) =>
            (s1, Left(e))
          case (s1, Right(a1)) =>
            val (s2, as) = repeatStep(rep.value, ps, s1, Nil)
            (s2, Right(::(a1, as.reverse)))
        }
        res2

      case sep: Grammar.GADT.Sep[S, S, E, ta, ts] =>
        val checkpoint = ps.i
        val (s1, res1): (S, E \/ ta) = step(sep.value, ps, s)
        val res2: (S, E \/ SeparatedBy[ta, ts]) = res1 match {
          case Left(_) =>
            ps.i = checkpoint
            (s1, Right(SeparatedBy()))
          case Right(a1) =>
            val (s2, as) = repeatStep(sep.sep, sep.value, ps, s1, SeparatedBy(a1))
            (s2, Right(as.reverse))
        }
        res2
    }

  @tailrec
  private def repeatStep[S, E, A](g: Grammar[S, S, E, A], ps: ParserState[E], s: S, as: List[A]): (S, List[A]) = {
    val checkpoint = ps.i
    step(g, ps, s) match {
      case (s1, Left(_))  => ps.i = checkpoint; (s1, as)
      case (s1, Right(a)) => repeatStep(g, ps, s1, a :: as)
    }
  }

  @tailrec
  private def repeatStep[S, E, A, B](g1: Grammar[S, S, E, B], g2: Grammar[S, S, E, A], ps: ParserState[E], s: S, as: SeparatedBy1[A, B]): (S, SeparatedBy1[A, B]) = {
    val checkpoint = ps.i
    step(g1, ps, s) match {
      case (s1, Left(_))  => ps.i = checkpoint; (s1, as)
      case (s1, Right(b)) => step(g2, ps, s1) match {
        case (s2, Left(_))  => ps.i = checkpoint; (s2, as)
        case (s2, Right(a)) => repeatStep(g1, g2, ps, s2, as.prepend(a, b))
      }
    }
  }
}
