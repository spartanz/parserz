package scalaz.parsers

import scalaz.parsers.tc.{ Alternative, Category }
import scalaz.{ Applicative, ApplicativeError, Cofree, Foldable, MonadError, Monoid, PlusEmpty, ~> }

object Parsing {

  def apply[F[_], G[_], E]()(
    implicit FE: ApplicativeError[F, E],
    GE: ApplicativeError[G, E],
    C1: Category[λ[(α, β) => α => F[β]]],
    C2: Category[λ[(α, β) => α => G[β]]]
  ): Parsing[F, G, E] =
    new Parsing[F, G, E] {
      override val F: ApplicativeError[F, E]             = FE
      override val G: ApplicativeError[G, E]             = GE
      override val AB: Transform[λ[(α, β) => α => F[β]]] = Transform(F, C1)
      override val BA: Transform[λ[(α, β) => α => G[β]]] = Transform(G, C2)
    }

  def apply[F[_], G[_], E](F_ : MonadError[F, E], G_ : MonadError[G, E]): Parsing[F, G, E] =
    new Parsing[F, G, E] {
      override val F: ApplicativeError[F, E]             = F_
      override val G: ApplicativeError[G, E]             = G_
      override val AB: Transform[λ[(α, β) => α => F[β]]] = Transform(F_)
      override val BA: Transform[λ[(α, β) => α => G[β]]] = Transform(G_)
    }
}

sealed trait Parsing[F[_], G[_], E] {

  protected val F: ApplicativeError[F, E]
  protected val G: ApplicativeError[G, E]
  protected val AB: Transform[λ[(α, β) => α => F[β]]]
  protected val BA: Transform[λ[(α, β) => α => G[β]]]

  object syntax extends EquivSyntax

  sealed trait Equiv[A, B] {
    def to: A => F[B]
    def from: B => G[A]
  }

  type ↻[R[_], B] = Cofree[Option, R[B]]

  sealed trait Recurse[R[_], B] extends Equiv[↻[R, B], B]

  object Recurse {
    import syntax._

    def apply[R[_], B](rb: ↻[R, B] => F[B], br: B => G[↻[R, B]]): Recurse[R, B] =
      new Recurse[R, B] {
        override def to: ↻[R, B] => F[B]   = rb
        override def from: B => G[↻[R, B]] = br
      }
  }

  object Equiv {
    import syntax._

    def apply[A, B](ab: A => F[B], ba: B => G[A]): Equiv[A, B] =
      new Equiv[A, B] {
        override def to: A => F[B]   = ab
        override def from: B => G[A] = ba
      }

    def lift[A, B](ab: A => B, ba: B => A): Equiv[A, B] =
      id[A].imap(ab, ba)

    def liftFG[A, B](ab: A => F[B], ba: B => G[A]): Equiv[A, B] =
      Equiv(ab, ba)

    def id[A]: Equiv[A, A] =
      Equiv(AB.id, BA.id)

    def ignore[A](a: A): Equiv[A, Unit] =
      lift(_ => (), _ => a)

    def create[A](a: A): Equiv[Unit, A] =
      lift(_ => a, _ => ())

    def ensure[A](e: E)(p: A => Boolean): Equiv[A, A] =
      liftFG(
        a => if (p(a)) F.pure(a) else F.raiseError(e),
        b => if (p(b)) G.pure(b) else G.raiseError(e)
      )

    def liftPartial[A, B](e: E)(ab: PartialFunction[A, B], ba: PartialFunction[B, A]): Equiv[A, B] =
      liftFG(
        a => ab.lift(a).fold[F[B]](F.raiseError(e))(F.pure(_)),
        b => ba.lift(b).fold[G[A]](G.raiseError(e))(G.pure(_))
      )

    def liftPartialF[A, B](
      e: E
    )(ab: PartialFunction[A, F[B]], ba: PartialFunction[B, G[A]]): Equiv[A, B] =
      liftFG(
        a => ab.lift(a).getOrElse(F.raiseError(e)),
        b => ba.lift(b).getOrElse(G.raiseError(e))
      )

    def nil[A](e: E): Equiv[Unit, List[A]] =
      liftPartial(e)(
        { case ()  => Nil },
        { case Nil => () }
      )

    // todo: return Equiv[(A, List[A]), NonEmptyList[A]]
    def nel[A](e: E): Equiv[A /\ List[A], List[A]] =
      liftPartial(e)(
        { case (x, xs) => x :: xs },
        { case x :: xs => (x, xs) }
      )

    /**
    def cofreeUnit[S[_], A](implicit S: PlusEmpty[S]): Equiv[Unit, S[Cofree[S, A]]] = lift(
      { case () => S.empty[Cofree[S, A]] },
      { case _  => () }
    )

    def cofreeNormUnit[S[_], A](implicit S: PlusEmpty[S]): Equiv[Unit, Unit \/ Cofree[S, A]] = lift(
      { case () => Left(Unit) },
      { case _  => () }
    )

    def cofree[S[_], A]: Equiv[A /\ S[Cofree[S, A]], Cofree[S, A]] =
      lift(
        { case (a, s) => Cofree(a, s) }, {
          case c      => c.head -> c.tail
        }
      )

    def cofreeNorm[S[_], A](a: A)(
      implicit S: PlusEmpty[S],
      SA: Applicative[S]
    ): Equiv[A /\ S[Cofree[S, A]], Unit \/ Cofree[S, A]] =
      lift(
        { case (a, s)   => Right(Cofree(a, s)) }, {
          case Left(_)  => a -> S.empty[Cofree[S, A]]
          case Right(c) => a -> SA.point(c)
        }
      )
    **/

    def list[A]: Equiv[A /\ List[A] \/ Unit, List[A]] = lift(
      {
        case Right(_)      => Nil
        case Left((a, as)) => a :: as
      }, {
        case Nil     => Right(())
        case a :: as => Left(a -> as)
      }
    )

    def iterate[A](equiv: Equiv[A, A])(implicit F0: Foldable[F], G0: Foldable[G]): Equiv[A, A] = {
      def step[L[_]](f: A => L[A], state: A)(implicit L: Foldable[L]): A =
        L.foldLeft(f(state), state) { case (_, s) => step(f, s) }

      lift(step(equiv.to, _), step(equiv.from, _))
    }

    /**
    def iterateK[S[_, _], T[_, _], A, B](
      equiv: Equiv[S[A, B], T[A, B]]
    )(implicit F0: Foldable[F], G0: Foldable[G]): Equiv[S[A, B], T[A, B]] = {
      def step[L[_]](f: S[A, B] => L[S[A, B]], state: S[A, B])(implicit L: Foldable[L]): T[A, B] =
        L.foldLeft(f(state), state) { case (_, s) => step(f, s) }

      lift(step(equiv.to, _), step(equiv.from, _))
    }
    **/
    /**
    def foldcf[S[_]: PlusEmpty, A, B](b: B, equiv: Equiv[A /\ B, A])(f: S ↻ B)(
      implicit F0: Foldable[F],
      G0: Foldable[G],
      FG: F ~> G,
      GF: G ~> F,
      SA: Applicative[S]
    ): Equiv[A /\ S[Cofree[S, B]], A] = {
      import Product._

      def step: Equiv[A ⓧ (Unit \/ Cofree[S, B]), A ⓧ S[Cofree[S, B]]] = {
        val first: Equiv[A ⓧ (Unit \/ Cofree[S, B]), A ⓧ (B ⓧ S[Cofree[S, B]])] =
          cofreeNorm[S, B](b).reverse.second[A]
        val app: Equiv[A ⓧ B ⓧ S[Cofree[S, B]], A ⓧ S[Cofree[S, B]]] = equiv.first

        first >>> associate >>> app
      }

      iterate(step) >>> cofreeNormUnit[S, B].second[A].reverse >>> unitR[A].reverse
    }
    **/
// (A /\ B) <=> (A /\ B, A)
    def foldl[A, B](e: E)(equiv: Equiv[A /\ B, A])(
      implicit F0: Foldable[F],
      G0: Foldable[G],
      FG: F ~> G,
      GF: G ~> F
    ): Equiv[A /\ List[B], A] = { // Return type:  Separated1
      import Product._
      def step: Equiv[A ⓧ List[B], A ⓧ List[B]] = {
        val first: Equiv[A ⓧ List[B], A ⓧ (B ⓧ List[B])] = nel[B](e).reverse.second
        val app: Equiv[A ⓧ B ⓧ List[B], A ⓧ List[B]]     = equiv.first
        first >>> associate >>> app
      }
      iterate(step) >>> nil[B](e).second[A].reverse >>> unitR[A].reverse
    }

    object Product {
      type ⓧ[A, B] = (A, B)
      type Id      = Unit

      def unitL[A]: Equiv[A, Id ⓧ A] =
        id[A].imap(((), _), _._2)

      def unitR[A]: Equiv[A, A ⓧ Id] =
        id[A].imap((_, ()), _._1)

      def commute[A, B]: Equiv[A ⓧ B, B ⓧ A] =
        Equiv(AB.swap, BA.swap)

      def associate[A, B, C]: Equiv[A ⓧ (B ⓧ C), A ⓧ B ⓧ C] =
        id[A ⓧ (B ⓧ C)].imap(
          { case (a, (b, c)) => ((a, b), c) },
          { case ((a, b), c) => (a, (b, c)) }
        )

      def flatten[A, B, C]: Equiv[A ⓧ (B ⓧ C), (A, B, C)] =
        id[A ⓧ (B ⓧ C)].imap(
          { case (a, (b, c)) => (a, b, c) },
          { case (a, b, c)   => (a, (b, c)) }
        )
    }
  }

  trait EquivSyntax {
    implicit final class ToEquivOps[A, B](self: Equiv[A, B]) {

      def >>> [C](that: Equiv[B, C]): Equiv[A, C] =
        Equiv(
          AB.compose(that.to, self.to),
          BA.compose(self.from, that.from)
        )

      //  def >>> [C](
      //    that: Equiv[B, C]
      //  )(implicit F: Bind[F], G: Bind[G]): Equiv[A, C] =
      //    new Equiv[A, C] {
      //      def to: A => F[C]   = a => F.flatMap(self.to(a))(that.to)
      //      def from: C => G[A] = c => G.flatMap(that.from(c))(self.from)
      //    }

      def <<< [C](that: Equiv[C, A]): Equiv[C, B] =
        that >>> self

      def imap[C](bc: B => C, cb: C => B): Equiv[A, C] =
        Equiv(
          AB.mapsnd(self.to)(bc),
          BA.mapfst(self.from)(cb)
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

      def ⓧ [C, D](that: Equiv[C, D]): Equiv[A /\ C, B /\ D] =
        /\(that)

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

      def unary_~(implicit FG: F ~> G, GF: G ~> F): Equiv[B, A] =
        reverse
    }
  }

  case class Codec[I, A](eq: Equiv[I, (I, A)]) { self =>
    import syntax._

    def parse(i: I): F[(I, A)] =
      eq.to(i)

    def print(a: A, initial: I): G[I] =
      eq.from(initial -> a)

    def print0(a: A)(implicit M: Monoid[I]): G[I] =
      eq.from(M.zero -> a)

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

    // IsoMonad functionality
    def ∘ [B](equiv: Equiv[A, B]): Codec[I, B] =
      Codec(
        self.eq >>> Equiv[(I, A), (I, B)](
          { case (i, a) => F.map(equiv.to(a))((i, _)) },
          { case (i, b) => G.map(equiv.from(b))((i, _)) }
        )
      )

    def many(implicit AF: Alternative[F]): Codec[I, List[A]] = {
      lazy val step: Codec[I, List[A]] =
        ((self ~ Codec(Equiv(step.eq.to(_), step.eq.from(_)))) | Codec.pure(())) ∘ Equiv.list
      step
    }

    // todo: return Codec[I, NonEmptyList[A]]
    def many1(e: E)(implicit AF: Alternative[F]): Codec[I, List[A]] =
      (self ~ many) ∘ Equiv.nel(e)
  }

  object Codec {

    def pure[I, A](a: A): Codec[I, A] =
      Codec(Equiv.lift[I, (I, A)]((_, a), _._1))
  }
}
