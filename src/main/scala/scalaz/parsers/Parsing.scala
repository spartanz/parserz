package scalaz.parsers

import scalaz.data.~>
import scalaz.tc._

object Parsing {

  def apply[F[_]: Applicative, G[_]: Applicative](
    implicit C1_ : Category[λ[(α, β) => α => F[β]]],
    C2_ : Category[λ[(α, β) => α => G[β]]]
  ): Parsing[F, G] =
    new Parsing[F, G] {
      override val F: Applicative[F]                    = implicitly
      override val G: Applicative[G]                    = implicitly
      override val C1: Category[λ[(α, β) => α => F[β]]] = implicitly
      override val C2: Category[λ[(α, β) => α => G[β]]] = implicitly
    }
}

sealed trait Parsing[F[_], G[_]] {

  protected val F: Applicative[F]
  protected val G: Applicative[G]
  protected val C1: Category[λ[(α, β) => α => F[β]]]
  protected val C2: Category[λ[(α, β) => α => G[β]]]

  sealed trait Equiv[A, B] {
    def to: A => F[B]
    def from: B => G[A]
  }

  object Equiv {
    private[parsers] val AB: Transform[λ[(α, β) => α => F[β]]] = Transform[F](F, C1)
    private[parsers] val BA: Transform[λ[(α, β) => α => G[β]]] = Transform[G](G, C2)

    def apply[A, B](ab: A => F[B], ba: B => G[A]): Equiv[A, B] =
      new Equiv[A, B] {
        override def to: A => F[B]   = ab
        override def from: B => G[A] = ba
      }

    def lift[A, B](ab: A => B, ba: B => A): Equiv[A, B] =
      Equiv(
        ab.andThen(AB.pure),
        ba.andThen(BA.pure)
      )

    def liftF[A, B](ab: A => F[B], ba: B => G[A]): Equiv[A, B] =
      Equiv(ab, ba)

    implicit class EquivOps[A, B](self: Equiv[A, B]) {

      def >>> [C](that: Equiv[B, C]): Equiv[A, C] =
        Equiv(
          AB.compose(that.to, self.to),
          BA.compose(self.from, that.from)
        )

      def imap[C](bc: B => C, cb: C => B): Equiv[A, C] =
        Equiv(
          AB.rmap(self.to)(bc),
          BA.lmap(self.from)(cb)
        )

      def first[C]: Equiv[(A, C), (B, C)] =
        Equiv(
          AB.first(self.to),
          BA.first(self.from)
        )

      def second[C]: Equiv[(C, A), (C, B)] =
        Equiv(
          AB.second(self.to),
          BA.second(self.from)
        )

      def /\ [C, D](that: Equiv[C, D]): Equiv[A /\ C, B /\ D] =
        Equiv(
          AB.conjunction(self.to, that.to),
          BA.conjunction(self.from, that.from)
        )

      def \/ [C, D](that: Equiv[C, D]): Equiv[A \/ C, B \/ D] =
        Equiv(
          AB.disjunction(self.to, that.to),
          BA.disjunction(self.from, that.from)
        )

      def reverse(implicit FG: F ~> G, GF: G ~> F): Equiv[B, A] =
        Equiv(
          b => GF.apply(self.from(b)),
          a => FG.apply(self.to(a))
        )
    }

    def list[A]: Equiv[A /\ List[A] \/ Unit, List[A]] = lift(
      {
        case Right(_)      => Nil
        case Left((a, as)) => a :: as
      }, {
        case Nil     => Right(())
        case a :: as => Left((a, as))
      }
    )

    def iterate[A](equiv: Equiv[A, A])(implicit F0: Foldable[F], G0: Foldable[G]): Equiv[A, A] = {
      def step[L[_]](f: A => L[A], state: A)(implicit L: Foldable[L]): A =
        L.foldLeft(f(state), state) { case (_, s) => step(f, s) }

      lift(step(equiv.to, _), step(equiv.from, _))
    }

    object Product {
      type ⓧ[A, B] = (A, B)
      type Id      = Unit

      def unitR[A]: Equiv[A, A ⓧ Id] =
        Equiv(
          a => AB.pure((a, ())),
          { case (a, _) => BA.pure(a) }
        )

      def associate[A, B, C]: Equiv[A ⓧ (B ⓧ C), A ⓧ B ⓧ C] =
        Equiv(
          { case (a, (b, c)) => AB.pure(((a, b), c)) },
          { case ((a, b), c) => BA.pure((a, (b, c))) }
        )
    }
  }

  case class Codec[I, A](eq: Equiv[I, (I, A)]) { self =>
    import Equiv._

    // ProductFunctor functionality
    def ~ [B](that: Codec[I, B]): Codec[I, (A, B)] =
      Codec(
        Equiv[I, (I, (A, B))](
          AB.compose[I, (I, A), (I, (A, B))](
            { case (i1, a) => F.map(that.eq.to(i1)) { case (i2, b) => (i2, (a, b)) } },
            self.eq.to
          ), {
            case (i, (a, b)) =>
              BA.compose[(I, A), I, I](
                i1 => that.eq.from((i1, b)),
                self.eq.from
              )((i, a))
          }
        )
      )

    // Alternative functionality
    def | [B](that: Codec[I, B])(implicit AF: Alternative[F]): Codec[I, A \/ B] =
      Codec(
        Equiv[I, (I, A \/ B)](
          i =>
            AF.or(
              F.map(self.eq.to(i)) { case (i1, a) => (i1, Left(a)) },
              F.map(that.eq.to(i)) { case (i2, b) => (i2, Right(b)) }
            ), {
            case (i, Left(a))  => self.eq.from((i, a))
            case (i, Right(b)) => that.eq.from((i, b))
          }
        )
      )

    // IsoFunctor functionality
    def ∘ [B](equiv: Equiv[A, B]): Codec[I, B] =
      Codec(
        self.eq >>> Equiv[(I, A), (I, B)](
          { case (i, a) => F.map(equiv.to(a))((i, _)) },
          { case (i, b) => G.map(equiv.from(b))((i, _)) }
        )
      )

    def many(implicit AF: Alternative[F]): Codec[I, List[A]] = {
      lazy val step: Codec[I, List[A]] =
        ((self ~ Codec(Equiv(step.eq.to(_), step.eq.from(_)))) | Codec.lift(())) ∘ Equiv.list
      step
    }
  }

  object Codec {
    import Equiv._

    def lift[I, A](a: A): Codec[I, A] =
      Codec(
        Equiv[I, (I, A)](
          i => AB.pure((i, a)),
          { case (i, _) => BA.pure(i) }
        )
      )
  }
}
