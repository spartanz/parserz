package scalaz.parsers.tc

trait Alternative[F[_]] {
  def or[A](f1: F[A], f2: => F[A]): F[A]
}

object Alternative {

  implicit val optionAlternative: Alternative[Option] =
    new Alternative[Option] {
      override def or[A](f1: Option[A], f2: => Option[A]): Option[A] =
        f1.orElse(f2)
    }

  implicit def eitherAlternative[R]: Alternative[Either[R, ?]] =
    new Alternative[Either[R, ?]] {
      override def or[A](f1: Either[R, A], f2: => Either[R, A]): Either[R, A] =
        f1 match {
          case Left(_)  => f2
          case Right(_) => f1
        }
    }
}
