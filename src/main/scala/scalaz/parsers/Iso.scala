package scalaz.parsers

import scalaz.data.~>
import scalaz.tc._

object IsoClass {

  def apply[F[_], G[_]](
    implicit F_ : Applicative[F],
    G_ : Applicative[G],
    C1_ : Category[λ[(α, β) => α => F[β]]],
    C2_ : Category[λ[(α, β) => α => G[β]]]
  ): IsoClass[F, G] =
    new IsoClass[F, G] {
      override val F: Applicative[F]                    = implicitly
      override val G: Applicative[G]                    = implicitly
      override val C1: Category[λ[(α, β) => α => F[β]]] = implicitly
      override val C2: Category[λ[(α, β) => α => G[β]]] = implicitly
    }
}

sealed trait IsoClass[F[_], G[_]] {

  implicit protected val F: Applicative[F]
  implicit protected val G: Applicative[G]
  implicit protected val C1: Category[λ[(α, β) => α => F[β]]]
  implicit protected val C2: Category[λ[(α, β) => α => G[β]]]

  sealed trait Iso[A, B] { self =>

    def to: A => F[B]
    def from: B => G[A]

    def >>> [C](that: Iso[B, C]): Iso[A, C] =
      new Iso[A, C] {
        def to: A => F[C]   = C1.compose(that.to, self.to)
        def from: C => G[A] = C2.compose(self.from, that.from)
      }

    //  def >>> [C](
    //    that: Iso[B, C]
    //  )(implicit F: Bind[F], G: Bind[G]): Iso[A, C] =
    //    new Iso[A, C] {
    //      def to: A => F[C]   = a => F.flatMap(self.to(a))(that.to)
    //      def from: C => G[A] = c => G.flatMap(that.from(c))(self.from)
    //    }

    def <<< [C](that: Iso[C, A]): Iso[C, B] =
      that >>> self

    def /\ [C, D](that: Iso[C, D]): Iso[A /\ C, B /\ D] =
      new Iso[A /\ C, B /\ D] {
        override def to: A /\ C => F[B /\ D] = {
          case (a, c) => F.ap(self.to(a))(F.map(that.to(c))(d => b => (b, d)))
        }
        override def from: B /\ D => G[A /\ C] = {
          case (b, d) => G.ap(self.from(b))(G.map(that.from(d))(c => a => (a, c)))
        }
      }

    def ⓧ [C, D](that: Iso[C, D]): Iso[A /\ C, B /\ D] =
      /\(that)

    def \/ [C, D](that: Iso[C, D]): Iso[A \/ C, B \/ D] =
      new Iso[A \/ C, B \/ D] {
        override def to: A \/ C => F[B \/ D] = {
          case Left(a)  => F.map(self.to(a))(Left(_))
          case Right(c) => F.map(that.to(c))(Right(_))
        }
        override def from: B \/ D => G[A \/ C] = {
          case Left(b)  => G.map(self.from(b))(Left(_))
          case Right(d) => G.map(that.from(d))(Right(_))
        }
      }

    def unary_~(implicit FG: F ~> G, GF: G ~> F): Iso[B, A] =
      new Iso[B, A] {
        override def to: B => F[A]   = b => GF.apply(self.from(b))
        override def from: A => G[B] = a => FG.apply(self.to(a))
      }
  }

  def pure[A]: Iso[A, A] =
    new Iso[A, A] {
      override def to: A => F[A]   = F.pure
      override def from: A => G[A] = G.pure
    }

  def lift[A, B](ab: A => B, ba: B => A): Iso[A, B] =
    liftF(
      ab.andThen(F.pure),
      ba.andThen(G.pure)
    )

  def liftF[A, B](ab: A => F[B], ba: B => G[A]): Iso[A, B] =
    new Iso[A, B] {
      override def to: A => F[B]   = ab
      override def from: B => G[A] = ba
    }

  object Product {

    type ⓧ[A, B] = (A, B)
    type Id      = Unit

    def unitL[A]: Iso[A, Id ⓧ A] =
      new Iso[A, Id ⓧ A] {
        override def to: A => F[Id ⓧ A]   = a => F.pure(((), a))
        override def from: Id ⓧ A => G[A] = { case (_, a) => G.pure(a) }
      }

    def unitR[A]: Iso[A, A ⓧ Id] =
      new Iso[A, A ⓧ Id] {
        override def to: A => F[A ⓧ Id]   = a => F.pure((a, ()))
        override def from: A ⓧ Id => G[A] = { case (a, _) => G.pure(a) }
      }

    def commute[A, B]: Iso[A ⓧ B, B ⓧ A] =
      new Iso[A ⓧ B, B ⓧ A] {
        override def to: A ⓧ B => F[B ⓧ A] = {
          case (a, b) => F.pure((b, a))
        }
        override def from: B ⓧ A => G[A ⓧ B] = {
          case (b, a) => G.pure((a, b))
        }
      }

    def associate[A, B, C]: Iso[A ⓧ (B ⓧ C), A ⓧ B ⓧ C] =
      new Iso[A ⓧ (B ⓧ C), A ⓧ B ⓧ C] {
        override def to: A ⓧ (B ⓧ C) => F[A ⓧ B ⓧ C] = {
          case (a, (b, c)) => F.pure(((a, b), c))
        }
        override def from: A ⓧ B ⓧ C => G[A ⓧ (B ⓧ C)] = {
          case ((a, b), c) => G.pure((a, (b, c)))
        }
      }

    def flatten[A, B, C]: Iso[A ⓧ (B ⓧ C), (A, B, C)] =
      new Iso[A ⓧ (B ⓧ C), (A, B, C)] {
        override def to: A ⓧ (B ⓧ C) => F[(A, B, C)] = {
          case (a, (b, c)) => F.pure((a, b, c))
        }
        override def from: ((A, B, C)) => G[A ⓧ (B ⓧ C)] = {
          case (a, b, c) => G.pure((a, (b, c)))
        }
      }
  }

  def ignore[A](a: A): Iso[A, Unit] =
    lift(_ => (), _ => a)

  def create[A](a: A): Iso[Unit, A] =
    lift(_ => a, _ => ())

  def list[A]: Iso[Unit \/ (A /\ List[A]), List[A]] = lift(
    {
      case Left(_)        => Nil
      case Right((a, as)) => a :: as
    }, {
      case Nil     => Left(())
      case a :: as => Right((a, as))
    }
  )

  def iterate[A](
    iso: Iso[A, A]
  )(
    implicit F0: Foldable[F],
    G0: Foldable[G]
  ): Iso[A, A] = {

    def step[L[_]](f: A => L[A], state: A)(implicit L: Foldable[L]): A =
      L.foldLeft(f(state), state) { case (_, s) => step(f, s) }

    lift(step(iso.to, _), step(iso.from, _))
  }
}
