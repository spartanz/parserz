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

  def /\ [C, D](
    that: Iso[F, G, C, D]
  )(implicit F: Applicative[F], G: Applicative[G]): Iso[F, G, A /\ C, B /\ D] =
    new Iso[F, G, A /\ C, B /\ D] {
      override def to: UFV[A /\ C, B /\ D] = {
        case (a, c) => F.ap(self.to(a))(F.map(that.to(c))(d => b => (b, d)))
      }
      override def from: UGV[B /\ D, A /\ C] = {
        case (b, d) => G.ap(self.from(b))(G.map(that.from(d))(c => a => (a, c)))
      }
    }

  def ⓧ [C, D](
    that: Iso[F, G, C, D]
  )(implicit F: Applicative[F], G: Applicative[G]): Iso[F, G, A /\ C, B /\ D] = /\(that)

  def \/ [C, D](
    that: Iso[F, G, C, D]
  )(implicit F: Applicative[F], G: Applicative[G]): Iso[F, G, A \/ C, B \/ D] =
    new Iso[F, G, A \/ C, B \/ D] {
      override def to: UFV[A \/ C, B \/ D] = {
        case Left(a)  => F.map(self.to(a))(Left(_))
        case Right(c) => F.map(that.to(c))(Right(_))
      }
      override def from: UGV[B \/ D, A \/ C] = {
        case Left(b)  => G.map(self.from(b))(Left(_))
        case Right(d) => G.map(that.from(d))(Right(_))
      }
    }

  def unary_~(implicit FG: F ~> G, GF: G ~> F): Iso[F, G, B, A] =
    new Iso[F, G, B, A] {
      override def to: UFV[B, A]   = b => GF.apply(self.from(b))
      override def from: UGV[A, B] = a => FG.apply(self.to(a))
    }
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

    def commute[A, B](implicit F: Applicative[F], G: Applicative[G]): Iso[F, G, A ⓧ B, B ⓧ A] =
      new Iso[F, G, A ⓧ B, B ⓧ A] {
        override def to: UFV[A ⓧ B, B ⓧ A] = {
          case (a, b) => F.pure((b, a))
        }
        override def from: UGV[B ⓧ A, A ⓧ B] = {
          case (b, a) => G.pure((a, b))
        }
      }

    def associate[A, B, C](
      implicit F: Applicative[F],
      G: Applicative[G]
    ): Iso[F, G, A ⓧ (B ⓧ C), A ⓧ B ⓧ C] =
      new Iso[F, G, A ⓧ (B ⓧ C), A ⓧ B ⓧ C] {
        override def to: UFV[A ⓧ (B ⓧ C), A ⓧ B ⓧ C] = {
          case (a, (b, c)) => F.pure(((a, b), c))
        }
        override def from: UGV[A ⓧ B ⓧ C, A ⓧ (B ⓧ C)] = {
          case ((a, b), c) => G.pure((a, (b, c)))
        }
      }

    def flatten[A, B, C](
      implicit F: Applicative[F],
      G: Applicative[G]
    ): Iso[F, G, A ⓧ (B ⓧ C), (A, B, C)] =
      new Iso[F, G, A ⓧ (B ⓧ C), (A, B, C)] {
        override def to: UFV[A ⓧ (B ⓧ C), (A, B, C)] = {
          case (a, (b, c)) => F.pure((a, b, c))
        }
        override def from: UGV[(A, B, C), A ⓧ (B ⓧ C)] = {
          case (a, b, c) => G.pure((a, (b, c)))
        }
      }
  }
}

trait IsoInstances0[F[_], G[_]] {
  self: IsoClass[F, G] =>

  def ignore[A](a: A)(implicit F: Applicative[F], G: Applicative[G]): Iso[F, G, A, Unit] =
    lift(_ => (), _ => a)

  def create[A](a: A)(implicit F: Applicative[F], G: Applicative[G]): Iso[F, G, Unit, A] =
    lift(_ => a, _ => ())

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
