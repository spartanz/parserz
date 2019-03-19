package scalaz.parsers

import scalaz.Scalaz.monadApplicative
import scalaz.parsers.implicits.monadKleisliCategory
import scalaz.tc.ProfunctorClass.DeriveDimap
import scalaz.tc._

trait CategorySyntax {
  implicit final class ToCategoryOps[=>:[_, _], B, C](self: B =>: C) {
    def ∘ [A](f: A =>: B)(implicit ev: CategoryClass[=>:]): A =>: C = ev.compose(self, f)
  }
}

object syntax extends CategorySyntax

trait TransformClass[=>:[_, _]] extends StrongClass[=>:] with CategoryClass[=>:] {

  implicit private val cc: CategoryClass[=>:] = this

  import syntax._

  def duplicate[A]: A =>: (A /\ A)

  def swap[A, B]: (A /\ B) =>: (B /\ A)
  def swap2[A, B, C]: (A /\ (B /\ C)) =>: (B /\ (A /\ C))

  def dropFirst[A, B]: (A /\ B) =>: B
  def dropSecond[A, B]: (A /\ B) =>: A

  def combine[A, B, C](ab: A =>: B, ac: A =>: C): A =>: (B /\ C)

  def cross[A, B, C, D](ab: A =>: B, cd: C =>: D): (A /\ C) =>: (C /\ B, A /\ D) =
    combine(swap ∘ first[A, B, C](ab), second(cd))

  def curl[A, B, X]: (X /\ (A /\ B)) =>: (B /\ A)

  // todo: extend ChoiceClass[=>:] when scalaz.Disjunction is used
  def leftchoice[A, B, C](pab: A =>: B): (A \/ C) =>: (B \/ C)
  def rightchoice[A, B, C](pab: A =>: B): (C \/ A) =>: (C \/ B)

  def conjunction[A, B, C, D](ab: A =>: B, cd: C =>: D): (A /\ C) =>: (B /\ D)
  def disjunction[A, B, C, D](ab: A =>: B, cd: C =>: D): (A \/ C) =>: (B \/ D)
}

object Transform {

  def apply[F[_]](
    implicit F: Applicative[F],
    C: Category[λ[(α, β) => α => F[β]]]
  ): Transform[λ[(α, β) => α => F[β]]] = instanceOf(
    new TransformClass[λ[(α, β) => α => F[β]]] with DeriveDimap[λ[(α, β) => α => F[β]]] {

      override def id[A]: A => F[A]                                        = C.id
      override def compose[A, B, C](f: B => F[C], g: A => F[B]): A => F[C] = C.compose(f, g)

      override def lmap[A, B, C](fab: A => F[B])(ca: C => A): C => F[B] =
        c => fab(ca(c))
      override def rmap[A, B, C](fab: A => F[B])(bc: B => C): A => F[C] =
        a => F.map(fab(a))(bc)

      override def first[A, B, C](pab: A => F[B]): A /\ C => F[B /\ C] = {
        case (a, c) => F.map(pab(a))((_, c))
      }

      override def second[A, B, C](pab: A => F[B]): C /\ A => F[C /\ B] = {
        case (c, a) => F.map(pab(a))((c, _))
      }

      override def duplicate[A]: A => F[A /\ A] =
        a => F.pure((a, a))

      override def swap[A, B]: A /\ B => F[B /\ A] = {
        case (a, b) => F.pure((b, a))
      }
      override def swap2[A, B, C]: A /\ (B /\ C) => F[B /\ (A /\ C)] = {
        case (a, (b, c)) => F.pure((b, (a, c)))
      }

      override def dropFirst[A, B]: A /\ B => F[B] = {
        case (_, b) => F.pure(b)
      }
      override def dropSecond[A, B]: A /\ B => F[A] = {
        case (a, _) => F.pure(a)
      }

      override def combine[A, B, C](ab: A => F[B], ac: A => F[C]): A => F[B /\ C] =
        a => F.ap(ab(a))(F.map(ac(a))(c => b => (b, c)))

      override def curl[A, B, X]: X /\ (A /\ B) => F[B /\ A] = {
        case (_, (a, b)) => F.pure((b, a))
      }

      override def conjunction[A, B, C, D](ab: A => F[B], cd: C => F[D]): A /\ C => F[B /\ D] = {
        case (a, c) => F.ap(ab(a))(F.map(cd(c))(d => b => (b, d)))
      }

      override def leftchoice[A, B, C](ab: A => F[B]): A \/ C => F[B \/ C] = {
        case Left(a)  => F.map(ab(a))(Left(_))
        case Right(c) => F.pure(Right(c))
      }
      override def rightchoice[A, B, C](ab: A => F[B]): C \/ A => F[C \/ B] = {
        case Left(c)  => F.pure(Left(c))
        case Right(a) => F.map(ab(a))(Right(_))
      }

      override def disjunction[A, B, C, D](ab: A => F[B], cd: C => F[D]): A \/ C => F[B \/ D] = {
        case Left(a)  => F.map(ab(a))(Left(_))
        case Right(c) => F.map(cd(c))(Right(_))
      }
    }
  )

  def apply[F[_]](
    implicit F: Monad[F]
  ): Transform[λ[(α, β) => α => F[β]]] =
    apply[F](
      monadApplicative[F](implicitly),
      monadKleisliCategory[F](implicitly)
    )

  trait DeriveTransformFunctions[=>:[_, _]] extends TransformClass[=>:] {

    implicit private val cc: CategoryClass[=>:] = this

    import syntax._

    override def combine[A, B, C](ab: A =>: B, ac: A =>: C): A =>: (B /\ C) =
      first[A, B, C](ab) ∘ second[A, C, A](ac) ∘ duplicate[A]

    override def dropSecond[A, B]: (A /\ B) =>: A =
      compose[A /\ B, B /\ A, A](dropFirst, swap)

    override def swap2[A, B, C]: (A /\ (B /\ C)) =>: (B /\ (A /\ C)) = {
      val a = dropSecond[A, B /\ C]
      val b = dropSecond[B, C] ∘ dropFirst[A, B /\ C]
      val c = dropFirst[B, C] ∘ dropFirst[A, B /\ C]
      combine(b, combine(a, c))
    }

    override def curl[A, B, X]: (X /\ (A /\ B)) =>: (B /\ A) = swap ∘ dropFirst

    override def conjunction[A, B, C, D](ab: A =>: B, cd: C =>: D): (A /\ C) =>: (B /\ D) =
      curl[D, B, C] ∘ swap2[D, C, B] ∘ curl[C /\ B, D, A] ∘
        swap2[(C, B), A, D] ∘ cross(ab, cd)

  }
}
