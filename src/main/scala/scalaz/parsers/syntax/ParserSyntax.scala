package scalaz.parsers.syntax

import scalaz.Zip
import scalaz.parsers._
import scalaz.parsers.tc.Alternative

trait ParserSyntax[P[_], F[_], G[_], E] {

  val parsing: Parsing[F, G, E]

  def char: P[Char]

  def pure[A](a: A): P[A]

  def left[A, B](pa: P[A]): P[A \/ B]

  def right[A, B](pb: P[B]): P[A \/ B]

  def and[A, B](pa: P[A], pb: P[B])(implicit Z: Zip[P]): P[A /\ B] =
    Z.zip(pa, pb)

  def or[A, B](pa: P[A], pb: => P[B])(implicit A: Alternative[P]): P[A \/ B] =
    A.or(left(pa), right(pb))

  def imap[A, B](pa: P[A])(instance: parsing.Equiv[A, B]): P[B]

  def delay[A](pa: => P[A]): P[A]

  implicit final class ParserOps[A](p: P[A]) {

    def /\ [B](other: P[B])(implicit Z: Zip[P]): P[A /\ B] =
      and(p, other)

    def \/ [B](other: => P[B])(implicit A: Alternative[P]): P[A \/ B] =
      or(p, other)

    def || (other: => P[A])(implicit A: Alternative[P]): P[A] =
      A.or(p, other)

    def ∘ [B](instance: parsing.Equiv[A, B]): P[B] =
      imap(p)(instance)

    def many(implicit Z: Zip[P], A: Alternative[P]): P[List[A]] = {
      lazy val step: P[List[A]] = ((p /\ delay(step)) \/ pure(())) ∘ parsing.Equiv.list
      step
    }
  }
}
