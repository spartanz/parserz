package scalaz.parsers

import scalaz.data.~>
import scalaz.tc.ProfunctorClass.DeriveDimap
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

  implicit protected val F: Applicative[F]
  implicit protected val G: Applicative[G]
  implicit protected val C1: Category[λ[(α, β) => α => F[β]]]
  implicit protected val C2: Category[λ[(α, β) => α => G[β]]]

  type Transform[=>:[_, _]] = InstanceOf[TransformClass[=>:]]

  sealed trait TransformClass[=>:[_, _]] extends StrongClass[=>:] with CategoryClass[=>:] {

    def duplicate[A]: A =>: (A, A)
    def swap[A, B]: (A, B) =>: (B, A)
    def dropFirst[A, B]: (A, B) =>: B

    def dropSecond[A, B]: (A, B) =>: A =
      compose[(A, B), (B, A), A](dropFirst, swap)

    def combine[A, B, C](ab: A =>: B, ac: A =>: C): A =>: (B, C) =
      compose(compose(first[A, B, C](ab), second[A, C, A](ac)), duplicate[A])

    // todo: this is correct but insane
    def conjunction[A, B, C, D](ab: A =>: B, cd: C =>: D): (A, C) =>: (B, D) = {
      val x3: (A, C) =>: (C, B)                     = compose(swap, first[A, B, C](ab))
      val x4: (A, C) =>: (A, D)                     = second[C, D, A](cd)
      val x5: (A, C) =>: ((C, B), (A, D))           = combine(x3, x4)
      val x6: ((C, B), (A, D)) =>: (A, ((C, B), D)) = swap2[(C, B), A, D]
      val x7: (A, ((C, B), D)) =>: (D, (C, B)) =
        compose(swap[(C, B), D], dropFirst[A, ((C, B), D)])
      val x8: (D, (C, B)) =>: (C, (D, B)) = swap2[D, C, B]
      val x9: (C, (D, B)) =>: (B, D)      = compose(swap[D, B], dropFirst[C, (D, B)])
      val x0: (A, C) =>: (B, D)           = compose(compose(compose(compose(x9, x8), x7), x6), x5)
      x0
    }

    def disjunction[A, B, C, D](ab: A =>: B, cd: C =>: D): (A \/ C) =>: (B \/ D)

    def swap2[A, B, C]: (A, (B, C)) =>: (B, (A, C)) = {
      val a = dropSecond[A, (B, C)]
      val b = compose(dropSecond[B, C], dropFirst[A, (B, C)])
      val c = compose(dropFirst[B, C], dropFirst[A, (B, C)])
      combine(b, combine(a, c))
    }
  }

  private def transform[M[_]](
    implicit M: Applicative[M],
    C: Category[λ[(α, β) => α => M[β]]]
  ): Transform[λ[(α, β) => α => M[β]]] = instanceOf(
    new TransformClass[λ[(α, β) => α => M[β]]] with DeriveDimap[λ[(α, β) => α => M[β]]] {

      override def id[A]: A => M[A]                                        = C.id
      override def compose[A, B, C](f: B => M[C], g: A => M[B]): A => M[C] = C.compose(f, g)

      override def first[A, B, C](pab: A => M[B]): ((A, C)) => M[(B, C)] = {
        case (a, c) => M.map(pab(a))((_, c))
      }
      override def second[A, B, C](pab: A => M[B]): ((C, A)) => M[(C, B)] = {
        case (c, a) => M.map(pab(a))((c, _))
      }

      override def lmap[A, B, C](fab: A => M[B])(ca: C => A): C => M[B] =
        c => fab(ca(c))
      override def rmap[A, B, C](fab: A => M[B])(bc: B => C): A => M[C] =
        a => M.map(fab(a))(bc)

      override def duplicate[A]: A => M[(A, A)] =
        a => M.pure((a, a))
      override def swap[A, B]: ((A, B)) => M[(B, A)] = {
        case (a, b) => M.pure((b, a))
      }
      override def dropFirst[A, B]: ((A, B)) => M[B] = {
        case (_, b) => M.pure(b)
      }
      override def dropSecond[A, B]: ((A, B)) => M[A] = {
        case (a, _) => M.pure(a)
      }

      override def combine[A, B, C](ab: A => M[B], ac: A => M[C]): A => M[(B, C)] =
        a => M.ap(ab(a))(M.map(ac(a))(c => b => (b, c)))

      override def conjunction[A, B, C, D](ab: A => M[B], cd: C => M[D]): ((A, C)) => M[(B, D)] = {
        case (a, c) => M.ap(ab(a))(M.map(cd(c))(d => b => (b, d)))
      }

      override def disjunction[A, B, C, D](ab: A => M[B], cd: C => M[D]): A \/ C => M[B \/ D] = {
        case Left(a)  => M.map(ab(a))(Left(_))
        case Right(c) => M.map(cd(c))(Right(_))
      }
    }
  )

  sealed trait Equiv[A, B] { self =>
    def to: A => F[B]
    def from: B => G[A]
  }

  object Equiv {
    private[parsers] val AB: Transform[λ[(α, β) => α => F[β]]] = transform[F]
    private[parsers] val BA: Transform[λ[(α, β) => α => G[β]]] = transform[G]

    def lift[A, B](ab: A => B, ba: B => A): Equiv[A, B] =
      liftF(
        ab.andThen(F.pure),
        ba.andThen(G.pure)
      )

    def liftF[A, B](ab: A => F[B], ba: B => G[A]): Equiv[A, B] = new Equiv[A, B] {
      def to: A => F[B]   = ab
      def from: B => G[A] = ba
    }

    implicit class EquivOps[A, B](self: Equiv[A, B]) {

      def >>> [C](that: Equiv[B, C]): Equiv[A, C] =
        new Equiv[A, C] {
          def to: A => F[C]   = AB.compose(that.to, self.to)
          def from: C => G[A] = BA.compose(self.from, that.from)
        }

      def imap[C](bc: B => C, cb: C => B): Equiv[A, C] =
        new Equiv[A, C] {
          override def to: A => F[C]   = AB.rmap(self.to)(bc)
          override def from: C => G[A] = BA.lmap(self.from)(cb)
        }

      def first[C]: Equiv[(A, C), (B, C)] =
        new Equiv[(A, C), (B, C)] {
          override def to: ((A, C)) => F[(B, C)]   = AB.first(self.to)
          override def from: ((B, C)) => G[(A, C)] = BA.first(self.from)
        }

      def second[C]: Equiv[(C, A), (C, B)] =
        new Equiv[(C, A), (C, B)] {
          override def to: ((C, A)) => F[(C, B)]   = AB.second(self.to)
          override def from: ((C, B)) => G[(C, A)] = BA.second(self.from)
        }

      def /\ [C, D](that: Equiv[C, D]): Equiv[A /\ C, B /\ D] =
        new Equiv[A /\ C, B /\ D] {
          override def to: A /\ C => F[B /\ D]   = AB.conjunction(self.to, that.to)
          override def from: B /\ D => G[A /\ C] = BA.conjunction(self.from, that.from)
        }

      def \/ [C, D](that: Equiv[C, D]): Equiv[A \/ C, B \/ D] =
        new Equiv[A \/ C, B \/ D] {
          override def to: A \/ C => F[B \/ D]   = AB.disjunction(self.to, that.to)
          override def from: B \/ D => G[A \/ C] = BA.disjunction(self.from, that.from)
        }

      def reverse(implicit FG: F ~> G, GF: G ~> F): Equiv[B, A] =
        new Equiv[B, A] {
          override def to: B => F[A]   = b => GF.apply(self.from(b))
          override def from: A => G[B] = a => FG.apply(self.to(a))
        }
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
        new Equiv[A, A ⓧ Id] {
          override def to: A => F[A ⓧ Id]   = a => F.pure((a, ()))
          override def from: A ⓧ Id => G[A] = { case (a, _) => G.pure(a) }
        }

      def associate[A, B, C]: Equiv[A ⓧ (B ⓧ C), A ⓧ B ⓧ C] =
        new Equiv[A ⓧ (B ⓧ C), A ⓧ B ⓧ C] {
          override def to: A ⓧ (B ⓧ C) => F[A ⓧ B ⓧ C] = {
            case (a, (b, c)) => F.pure(((a, b), c))
          }
          override def from: A ⓧ B ⓧ C => G[A ⓧ (B ⓧ C)] = {
            case ((a, b), c) => G.pure((a, (b, c)))
          }
        }
    }
  }

  case class Codec[I, A](eq: Equiv[I, (I, A)]) { self =>
    import Equiv._

    // ProductFunctor functionality
    def ~ [B](that: Codec[I, B]): Codec[I, (A, B)] =
      Codec(new Equiv[I, (I, (A, B))] {
        override def to: I => F[(I, (A, B))] =
          AB.compose[I, (I, A), (I, (A, B))](
            { case (i1, a) => F.map(that.eq.to(i1)) { case (i2, b) => (i2, (a, b)) } },
            self.eq.to
          )
        override def from: ((I, (A, B))) => G[I] = {
          case (i, (a, b)) =>
            BA.compose[(I, A), I, I](
              i1 => that.eq.from((i1, b)),
              self.eq.from
            )((i, a))
        }
      })

    // Alternative functionality
    def | [B](that: Codec[I, B])(implicit AF: Alternative[F]): Codec[I, A \/ B] =
      Codec(new Equiv[I, (I, A \/ B)] {
        override def to: I => F[(I, A \/ B)] =
          i =>
            AF.or(
              F.map(self.eq.to(i)) { case (i1, a) => (i1, Left(a)) },
              F.map(that.eq.to(i)) { case (i2, b) => (i2, Right(b)) }
            )
        override def from: ((I, A \/ B)) => G[I] = {
          case (i, Left(a))  => self.eq.from((i, a))
          case (i, Right(b)) => that.eq.from((i, b))
        }
      })

    // IsoFunctor functionality
    def ∘ [B](equiv: Equiv[A, B]): Codec[I, B] =
      Codec(new Equiv[I, (I, B)] {
        override def to: I => F[(I, B)] =
          AB.compose[I, (I, A), (I, B)](
            { case (i, a) => F.map(equiv.to(a))((i, _)) },
            self.eq.to
          )
        override def from: ((I, B)) => G[I] =
          BA.compose[(I, B), (I, A), I](
            self.eq.from,
            { case (i, b) => G.map(equiv.from(b))((i, _)) }
          )
      })

    def many(implicit AF: Alternative[F]): Codec[I, List[A]] = {
      lazy val step: Codec[I, List[A]] =
        ((self ~ Codec(
          new Equiv[I, (I, List[A])] {
            override def to: I => F[(I, List[A])]     = step.eq.to(_)
            override def from: ((I, List[A])) => G[I] = step.eq.from(_)
          }
        )) | Codec.lift(())) ∘ Equiv.list
      step
    }
  }

  object Codec {

    def lift[I, A](a: A): Codec[I, A] =
      Codec(new Equiv[I, (I, A)] {
        override def to: I => F[(I, A)]     = i => F.pure((i, a))
        override def from: ((I, A)) => G[I] = { case (i, _) => G.pure(i) }
      })
  }
}
