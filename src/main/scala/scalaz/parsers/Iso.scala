package scalaz.parsers

import scalaz.data.~>
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

trait IsoClass[F[_], G[_]] extends IsoInstances0[F, G] {

  def id[A](implicit F: Applicative[F], G: Applicative[G]): Iso[F, G, A, A] =
    new Iso[F, G, A, A] {
      override def to: UFV[A, A]   = F.pure
      override def from: UGV[A, A] = G.pure
    }

  def lift[A, B](
    ab: A => B,
    ba: B => A
  )(implicit F: Applicative[F], G: Applicative[G]): Iso[F, G, A, B] =
    liftF(
      ab.andThen(F.pure),
      ba.andThen(G.pure)
    )

  def liftF[A, B](ab: A => F[B], ba: B => G[A]): Iso[F, G, A, B] =
    new Iso[F, G, A, B] {
      override def to: UFV[A, B]   = ab
      override def from: UGV[B, A] = ba
    }

  object Product {

    type ⓧ[A, B] = (A, B)
    type Id      = Unit

    def unitL[A](implicit F: Applicative[F], G: Applicative[G]): Iso[F, G, A, Id ⓧ A] =
      new Iso[F, G, A, Id ⓧ A] {
        override def to: UFV[A, Id ⓧ A]   = a => F.pure(((), a))
        override def from: UGV[Id ⓧ A, A] = { case (_, a) => G.pure(a) }
      }

    def unitR[A](implicit F: Applicative[F], G: Applicative[G]): Iso[F, G, A, A ⓧ Id] =
      new Iso[F, G, A, A ⓧ Id] {
        override def to: UFV[A, A ⓧ Id]   = a => F.pure((a, ()))
        override def from: UGV[A ⓧ Id, A] = { case (a, _) => G.pure(a) }
      }

    def associate[A, B, C](
      implicit F: Applicative[F],
      G: Applicative[G]
    ): Iso[F, G, (A, (B, C)), ((A, B), C)] =
      new Iso[F, G, (A, (B, C)), ((A, B), C)] {
        override def to: UFV[(A, (B, C)), ((A, B), C)] = {
          case (a, (b, c)) => F.pure(((a, b), c))
        }
        override def from: UGV[((A, B), C), (A, (B, C))] = {
          case ((a, b), c) => G.pure((a, (b, c)))
        }
      }

    def flatten[A, B, C](
      implicit F: Applicative[F],
      G: Applicative[G]
    ): Iso[F, G, (A, (B, C)), (A, B, C)] =
      new Iso[F, G, (A, (B, C)), (A, B, C)] {
        override def to: UFV[(A, (B, C)), (A, B, C)] = {
          case (a, (b, c)) => F.pure((a, b, c))
        }
        override def from: UGV[(A, B, C), (A, (B, C))] = {
          case (a, b, c) => G.pure((a, (b, c)))
        }
      }
  }
}

trait IsoInstances0[F[_], G[_]] {
  self: IsoClass[F, G] =>

  def list[A](
    implicit F: Applicative[F],
    G: Applicative[G]
  ): Iso[F, G, Unit \/ (A /\ List[A]), List[A]] = lift(
    {
      case Left(_)        => Nil
      case Right((a, as)) => a :: as
    }, {
      case Nil     => Left(())
      case a :: as => Right((a, as))
    }
  )
}

object Combinators {

  implicit class IsoOps[F[_], G[_], A, B](ab: Iso[F, G, A, B]) {

    def ∘ [C](
      ca: Iso[F, G, C, A]
    )(implicit C1: Category[ab.UFV], C2: Category[ab.UGV]): Iso[F, G, C, B] = ca >>> ab

    def | (
      abOther: Iso[F, G, A, B]
    )(AF: Alternative[F], AG: Alternative[G]): Iso[F, G, A, B] =
      new Iso[F, G, A, B] {
        override def to: UFV[A, B]   = (a: A) => AF.or(ab.to(a), abOther.to(a))
        override def from: UGV[B, A] = (b: B) => AG.or(ab.from(b), abOther.from(b))
      }

    def unary_~(implicit FG: F ~> G, GF: G ~> F): Iso[F, G, B, A] =
      new Iso[F, G, B, A] {
        override def to: UFV[B, A]   = b => GF.apply(ab.from(b))
        override def from: UGV[A, B] = a => FG.apply(ab.to(a))
      }

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

    def commute(implicit AF: Applicative[F], AG: Applicative[G]): Iso[F, G, (A, B), (B, A)] =
      new Iso[F, G, (A, B), (B, A)] {
        override def to: UFV[(A, B), (B, A)] = {
          case (a, b) => AF.pure(b -> a)
        }

        override def from: UGV[(B, A), (A, B)] = {
          case (b, a) => AG.pure(a -> b)
        }
      }
  }
}
