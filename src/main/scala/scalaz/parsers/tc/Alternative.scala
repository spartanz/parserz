package scalaz.parsers.tc

import scalaz.{ EitherT, Monad }

trait Alternative[F[_]] {
  def or[A](f1: F[A], f2: => F[A]): F[A]
}

object Alternative {

  implicit val optionAlternative: Alternative[Option] =
    new Alternative[Option] {
      override def or[A](f1: Option[A], f2: => Option[A]): Option[A] =
        f1.orElse(f2)
    }

  implicit def eitherAlternative[L]: Alternative[Either[L, ?]] =
    new Alternative[Either[L, ?]] {
      override def or[A](f1: Either[L, A], f2: => Either[L, A]): Either[L, A] =
        f1 match {
          case Left(_)  => f2
          case Right(_) => f1
        }
    }

  implicit def eitherTAlternative[F[_]: Monad, L]: Alternative[EitherT[L, F, ?]] =
    new Alternative[EitherT[L, F, ?]] {
      override def or[A](f1: EitherT[L, F, A], f2: => EitherT[L, F, A]): EitherT[L, F, A] =
        f1.orElse(f2)
    }
}
