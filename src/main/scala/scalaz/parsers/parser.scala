package scalaz.parsers

import scalaz.tc.{ Applicative, Functor }

trait ParserSyntax[P[_]] {

  def delay[A](pa: => P[A]): P[A]

  def lift[A](a: A)(implicit P: Applicative[P]): P[A] =
    P.pure(a)

  implicit class ParserOps[A](p: P[A]) {

    def /\ [B](other: P[B])(implicit P: Applicative[P]): P[A /\ B] =
      P.ap(p)(P.ap(other)(P.pure[B => A => A /\ B](b => a => (a, b))))

    def \/ [B](other: => P[B])(implicit P: Functor[P], A: Alternative[P]): P[A \/ B] =
      A.or(P.map(p)(Left(_)), P.map(other)(Right(_)))

    def || (other: => P[A])(implicit A: Alternative[P]): P[A] =
      A.or(p, other)
  }
}

trait ParserIsoSyntax[P[_], F[_], G[_]] {
  self: ParserSyntax[P] =>

  val iso: IsoClass[F, G]

  def isoMap[A, B](pa: P[A])(iso: Iso[F, G, A, B]): P[B]

  implicit class ParserIsoOps[A](p: P[A]) {

    def ∘ [B](iso: Iso[F, G, A, B]): P[B] =
      isoMap(p)(iso)

    def many(
      implicit P: Applicative[P],
      A: Alternative[P],
      F: Applicative[F],
      G: Applicative[G]
    ): P[List[A]] = {
      import scalaz.Scalaz.{ applicativeApply, applyFunctor }
      lazy val step: P[List[A]] = (lift(()) \/ (p /\ delay(step))) ∘ iso.list
      step
    }
  }
}
