package scalaz.parsers

import scalaz.data.~>
import scalaz.tc._

sealed trait Iso[F[_], G[_], A, B] { self =>

  def to: A => F[B]
  def from: B => G[A]

  def >>> [C](
    that: Iso[F, G, B, C]
  )(
    implicit C1: Category[λ[(α, β) => α => F[β]]],
    C2: Category[λ[(α, β) => α => G[β]]]
  ): Iso[F, G, A, C] =
    new Iso[F, G, A, C] {
      def to: A => F[C]   = C1.compose(that.to, self.to)
      def from: C => G[A] = C2.compose(self.from, that.from)
    }

  //  def >>> [C](
  //    that: Iso[F, G, B, C]
  //  )(implicit F: Bind[F], G: Bind[G]): Iso[F, G, A, C] =
  //    new Iso[F, G, A, C] {
  //      def to: A => F[C]   = a => F.flatMap(self.to(a))(that.to)
  //      def from: C => G[A] = c => G.flatMap(that.from(c))(self.from)
  //    }

  def <<< [C](
    that: Iso[F, G, C, A]
  )(
    implicit C1: Category[λ[(α, β) => α => F[β]]],
    C2: Category[λ[(α, β) => α => G[β]]]
  ): Iso[F, G, C, B] =
    that >>> self

  def /\ [C, D](
    that: Iso[F, G, C, D]
  )(implicit F: Applicative[F], G: Applicative[G]): Iso[F, G, A /\ C, B /\ D] =
    new Iso[F, G, A /\ C, B /\ D] {
      override def to: A /\ C => F[B /\ D] = {
        case (a, c) => F.ap(self.to(a))(F.map(that.to(c))(d => b => (b, d)))
      }
      override def from: B /\ D => G[A /\ C] = {
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
      override def to: A \/ C => F[B \/ D] = {
        case Left(a)  => F.map(self.to(a))(Left(_))
        case Right(c) => F.map(that.to(c))(Right(_))
      }
      override def from: B \/ D => G[A \/ C] = {
        case Left(b)  => G.map(self.from(b))(Left(_))
        case Right(d) => G.map(that.from(d))(Right(_))
      }
    }

  def unary_~(implicit FG: F ~> G, GF: G ~> F): Iso[F, G, B, A] =
    new Iso[F, G, B, A] {
      override def to: B => F[A]   = b => GF.apply(self.from(b))
      override def from: A => G[B] = a => FG.apply(self.to(a))
    }
}

trait IsoClass[F[_], G[_]] {

  def id[A](implicit F: Applicative[F], G: Applicative[G]): Iso[F, G, A, A] =
    new Iso[F, G, A, A] {
      override def to: A => F[A]   = F.pure
      override def from: A => G[A] = G.pure
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
      override def to: A => F[B]   = ab
      override def from: B => G[A] = ba
    }

  object Product {

    type ⓧ[A, B] = (A, B)
    type Id      = Unit

    def unitL[A](implicit F: Applicative[F], G: Applicative[G]): Iso[F, G, A, Id ⓧ A] =
      new Iso[F, G, A, Id ⓧ A] {
        override def to: A => F[Id ⓧ A]   = a => F.pure(((), a))
        override def from: Id ⓧ A => G[A] = { case (_, a) => G.pure(a) }
      }

    def unitR[A](implicit F: Applicative[F], G: Applicative[G]): Iso[F, G, A, A ⓧ Id] =
      new Iso[F, G, A, A ⓧ Id] {
        override def to: A => F[A ⓧ Id]   = a => F.pure((a, ()))
        override def from: A ⓧ Id => G[A] = { case (a, _) => G.pure(a) }
      }

    def commute[A, B](implicit F: Applicative[F], G: Applicative[G]): Iso[F, G, A ⓧ B, B ⓧ A] =
      new Iso[F, G, A ⓧ B, B ⓧ A] {
        override def to: A ⓧ B => F[B ⓧ A] = {
          case (a, b) => F.pure((b, a))
        }
        override def from: B ⓧ A => G[A ⓧ B] = {
          case (b, a) => G.pure((a, b))
        }
      }

    def associate[A, B, C](
      implicit F: Applicative[F],
      G: Applicative[G]
    ): Iso[F, G, A ⓧ (B ⓧ C), A ⓧ B ⓧ C] =
      new Iso[F, G, A ⓧ (B ⓧ C), A ⓧ B ⓧ C] {
        override def to: A ⓧ (B ⓧ C) => F[A ⓧ B ⓧ C] = {
          case (a, (b, c)) => F.pure(((a, b), c))
        }
        override def from: A ⓧ B ⓧ C => G[A ⓧ (B ⓧ C)] = {
          case ((a, b), c) => G.pure((a, (b, c)))
        }
      }

    def flatten[A, B, C](
      implicit F: Applicative[F],
      G: Applicative[G]
    ): Iso[F, G, A ⓧ (B ⓧ C), (A, B, C)] =
      new Iso[F, G, A ⓧ (B ⓧ C), (A, B, C)] {
        override def to: A ⓧ (B ⓧ C) => F[(A, B, C)] = {
          case (a, (b, c)) => F.pure((a, b, c))
        }
        override def from: ((A, B, C)) => G[A ⓧ (B ⓧ C)] = {
          case (a, b, c) => G.pure((a, (b, c)))
        }
      }
  }

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

  def iterate[A](
    iso: Iso[F, G, A, A]
  )(
    implicit F: Applicative[F],
    G: Applicative[G],
    F0: Foldable[F],
    G0: Foldable[G]
  ): Iso[F, G, A, A] = {

    def step[L[_]](f: A => L[A], state: A)(implicit L: Foldable[L]): A =
      L.foldLeft(f(state), state) { case (_, s) => step(f, s) }

    lift(step(iso.to, _), step(iso.from, _))
  }
}
