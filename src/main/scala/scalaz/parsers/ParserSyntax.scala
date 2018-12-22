package scalaz.parsers

import scalaz.tc.Applicative

trait ParserSyntax[P[_], F[_], G[_]] {

  val iso: IsoClass[F, G]

  def char: P[Char]

  def lift[A](a: A): P[A]

  def left[A, B](pa: P[A]): P[A \/ B]

  def right[A, B](pb: P[B]): P[A \/ B]

  def and[A, B](pa: P[A], pb: P[B])(implicit P: ProductFunctor[P]): P[A /\ B] =
    P.and(pa, pb)

  def or[A, B](pa: P[A], pb: => P[B])(implicit A: Alternative[P]): P[A \/ B] =
    A.or(left(pa), right(pb))

  def isoMap[A, B](pa: P[A])(iso: Iso[F, G, A, B]): P[B]

  def delay[A](pa: => P[A]): P[A]

  implicit class ParserOps[A](p: P[A]) {

    def /\ [B](other: P[B])(implicit P: ProductFunctor[P]): P[A /\ B] =
      and(p, other)

    def \/ [B](other: => P[B])(implicit A: Alternative[P]): P[A \/ B] =
      or(p, other)

    def || (other: => P[A])(implicit A: Alternative[P]): P[A] =
      A.or(p, other)

    def ∘ [B](iso: Iso[F, G, A, B]): P[B] =
      isoMap(p)(iso)

    def many(
      implicit P: ProductFunctor[P],
      A: Alternative[P],
      F: Applicative[F],
      G: Applicative[G]
    ): P[List[A]] = {
      lazy val step: P[List[A]] = (lift(()) \/ (p /\ delay(step))) ∘ iso.list
      step
    }
  }
}
