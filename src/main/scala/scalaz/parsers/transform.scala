package scalaz.parsers

import scalaz.Scalaz.monadApplicative
import scalaz.tc.ProfunctorClass.DeriveDimap
import scalaz.tc._

trait TransformClass[=>:[_, _]] extends StrongClass[=>:] with CategoryClass[=>:] {

  def duplicate[A]: A =>: (A /\ A)

  def swap[A, B]: (A /\ B) =>: (B /\ A)
  def swap2[A, B, C]: (A /\ (B /\ C)) =>: (B /\ (A /\ C))

  def dropFirst[A, B]: (A /\ B) =>: B
  def dropSecond[A, B]: (A /\ B) =>: A

  def combine[A, B, C](ab: A =>: B, ac: A =>: C): A =>: (B /\ C)

  // todo: extend ChoiceClass[=>:] when scalaz.Disjunction is used
  def leftchoice[A, B, C](pab: A =>: B): (A \/ C) =>: (B \/ C)
  def rightchoice[A, B, C](pab: A =>: B): (C \/ A) =>: (C \/ B)

  def pure[A]: A =>: A = id

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
      instanceOf(
        new CategoryClass[λ[(α, β) => α => F[β]]] {
          override def id[A]: A => F[A] =
            F.pure
          override def compose[A, B, C](f: B => F[C], g: A => F[B]): A => F[C] =
            a => F.flatMap(g(a))(f)
        }
      )
    )

  trait DeriveTransformFunctions[=>:[_, _]]
      extends TransformClass[=>:]
      with Alt[DeriveTransformFunctions[=>:]] {

    override def combine[A, B, C](ab: A =>: B, ac: A =>: C): A =>: (B /\ C) =
      compose(compose(first[A, B, C](ab), second[A, C, A](ac)), duplicate[A])

    override def dropSecond[A, B]: (A /\ B) =>: A =
      compose[A /\ B, B /\ A, A](dropFirst, swap)

    override def swap2[A, B, C]: (A /\ (B /\ C)) =>: (B /\ (A /\ C)) = {
      val a = dropSecond[A, B /\ C]
      val b = compose(dropSecond[B, C], dropFirst[A, B /\ C])
      val c = compose(dropFirst[B, C], dropFirst[A, B /\ C])
      combine(b, combine(a, c))
    }

    override def conjunction[A, B, C, D](ab: A =>: B, cd: C =>: D): (A /\ C) =>: (B /\ D) = {
      val x1: (A /\ C) =>: (C /\ B)                     = compose(swap, first[A, B, C](ab))
      val x2: (A /\ C) =>: (A /\ D)                     = second[C, D, A](cd)
      val x3: (A /\ C) =>: (C /\ B, A /\ D)             = combine(x1, x2)
      val x4: (C /\ B, A /\ D) =>: (A /\ (C /\ B /\ D)) = swap2[(C, B), A, D]
      val x5: (A /\ (C /\ B /\ D)) =>: (D /\ (C /\ B)) =
        compose(swap[C /\ B, D], dropFirst[A, C /\ B /\ D])
      val x6: (D /\ (C /\ B)) =>: (C /\ (D /\ B)) = swap2[D, C, B]
      val x7: (C /\ (D /\ B)) =>: (B /\ D)        = compose(swap[D, B], dropFirst[C, (D, B)])
      val x8: (A /\ C) =>: (B /\ D)               = compose(compose(compose(compose(x7, x6), x5), x4), x3)
      x8
    }
  }

  trait Alt[D <: Alt[D]]
}
