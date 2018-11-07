package scalaz.parsers

import scalaz.tc._

trait Iso[F[_], G[_], A, B] { self =>
  type UFV[U, V] = U => F[V]
  type UGV[U, V] = U => G[V]

  def to: UFV[A, B]
  def from: UGV[B, A]

  def >>> [C](
    that: Iso[F, G, B, C]
  )(implicit C1: Category[UFV], C2: Category[UGV]): Iso[F, G, A, C] =
    new Iso[F, G, A, C] {
      def to: UFV[A, C]   = C1.compose(that.to, self.to)
      def from: UGV[C, A] = C2.compose(self.from, that.from)
    }

  def <<< [C](
    that: Iso[F, G, C, A]
  )(implicit C1: Category[UFV], C2: Category[UGV]): Iso[F, G, C, B] = that >>> self
}

object Combinators {

  trait TempAlternative[F[_]] {
    def or[A](f1: F[A], f2: => F[A]): F[A]
  }

  implicit class IsoOps[F[_], G[_], A, B](ab: Iso[F, G, A, B]) {

    def ∘ [C](
      ca: Iso[F, G, C, A]
    )(implicit C1: Category[ab.UFV], C2: Category[ab.UGV]): Iso[F, G, C, B] = ca >>> ab

    def | (
      abOther: Iso[F, G, A, B]
    )(AF: TempAlternative[F], AG: TempAlternative[G]): Iso[F, G, A, B] =
      new Iso[F, G, A, B] {
        override def to: UFV[A, B]   = (a: A) => AF.or(ab.to(a), abOther.to(a))
        override def from: UGV[B, A] = (b: B) => AG.or(ab.from(b), abOther.from(b))
      }

//    def unary_~(implicit GF: G ~> F, FG: F ~> G): Iso[F, G, B, A] =
//      new Iso[F, G, B, A] {
//        override def to: UFV[B, A]   = (b: B) => GF.apply(ab.from(b))
//        override def from: UGV[A, B] = (a: A) => FG.apply(ab.to(a))
//      }

    def ⓧ [A2, B2](
      other: Iso[F, G, A2, B2]
    )(implicit AF: Applicative[F], AG: Applicative[G]): Iso[F, G, (A, A2), (B, B2)] =
      new Iso[F, G, (A, A2), (B, B2)] {
        override def to: UFV[(A, A2), (B, B2)] = {
          case (a, a2) => AF.ap(ab.to(a))(AF.map(other.to(a2))(b2 => (b: B) => (b, b2)))
        }

        override def from: UGV[(B, B2), (A, A2)] = {
          case (b, b2) => AG.ap(ab.from(b))(AG.map(other.from(b2))(a2 => (a: A) => (a, a2)))
        }
      }

    def liftA(implicit AF: Applicative[F], AG: Applicative[G]): Iso[F, G, A, A] =
      new Iso[F, G, A, A] {
        override def to: UFV[A, A] =
          a => AF.ap(AF.pure(a))(AF.pure[A => A](identity))

        override def from: UGV[A, A] =
          a => AG.ap(AG.pure(a))(AG.pure[A => A](identity))
      }

    def liftB(implicit AF: Applicative[F], AG: Applicative[G]): Iso[F, G, B, B] =
      new Iso[F, G, B, B] {
        override def to: UFV[B, B] =
          b => AF.ap(AF.pure(b))(AF.pure[B => B](identity))

        override def from: UGV[B, B] =
          a => AG.ap(AG.pure(a))(AG.pure[B => B](identity))
      }

    def unitA(a: A)(implicit AF: Applicative[F], AG: Applicative[G]): Iso[F, G, A, Unit] =
      new Iso[F, G, A, Unit] {
        override def to: UFV[A, Unit] =
          a0 => AF.ap[A, Unit](AF.pure(a0))(AF.pure[A => Unit](_ => ()))

        override def from: UGV[Unit, A] =
          _ => AG.ap[Unit, A](AG.pure(()))(AG.pure[Unit => A](_ => a))
      }

    def unitB(b: B)(implicit AF: Applicative[F], AG: Applicative[G]): Iso[F, G, B, Unit] =
      new Iso[F, G, B, Unit] {
        override def to: UFV[B, Unit] =
          b0 => AF.ap[B, Unit](AF.pure(b0))(AF.pure[B => Unit](_ => ()))

        override def from: UGV[Unit, B] =
          _ => AG.ap[Unit, B](AG.pure(()))(AG.pure[Unit => B](_ => b))
      }

    def associate[C](
      implicit AF: Applicative[F],
      AG: Applicative[G]
    ): Iso[F, G, (A, (B, C)), ((A, B), C)] = new Iso[F, G, (A, (B, C)), ((A, B), C)] {
      override def to: UFV[(A, (B, C)), ((A, B), C)] = {
        case (a, (b, c)) => AF.pure((a -> b) -> c)
      }

      override def from: UGV[((A, B), C), (A, (B, C))] = {
        case ((a, b), c) => AG.pure(a -> (b -> c))
      }
    }

    def commute(implicit AF: Applicative[F], AG: Applicative[G]): Iso[F, G, (A, B), (B, A)] =
      new Iso[F, G, (A, B), (B, A)] {
        override def to: UFV[(A, B), (B, A)] = {
          case (a, b) => AF.pure(b -> a)
        }

        override def from: UGV[(B, A), (A, B)] = {
          case (b, a) => AG.pure(a -> b)
        }
      }

    def unit(implicit AF: Applicative[F], AG: Applicative[G]): Iso[F, G, A, (A, Unit)] =
      new Iso[F, G, A, (A, Unit)] {
        override def to: UFV[A, (A, Unit)] =
          a => AF.pure((a, ()))

        override def from: UGV[(A, Unit), A] = {
          case (a, _) => AG.pure(a)
        }
      }

    def flatten[C](
      implicit AF: Applicative[F],
      AG: Applicative[G]
    ): Iso[F, G, (A, (B, C)), (A, B, C)] = new Iso[F, G, (A, (B, C)), (A, B, C)] {
      override def to: UFV[(A, (B, C)), (A, B, C)] = {
        case (a, (b, c)) => AF.pure((a, b, c))
      }

      override def from: UGV[(A, B, C), (A, (B, C))] = {
        case (a, b, c) => AG.pure(a -> (b -> c))
      }
    }

  }
}
