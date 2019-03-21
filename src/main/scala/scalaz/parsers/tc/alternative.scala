package scalaz.parsers.tc

import scalaz.parsers.Alternative
import scalaz.tc.instanceOf

trait AlternativeClass[F[_]] {
  def or[A](f1: F[A], f2: => F[A]): F[A]
}

object AlternativeClass {

  implicit val optionAlternative: Alternative[Option] =
    instanceOf(new AlternativeClass[Option] {
      override def or[A](f1: Option[A], f2: => Option[A]): Option[A] =
        f1.orElse(f2)
    })

  implicit def eitherAlternative[R]: Alternative[Either[R, ?]] =
    instanceOf(new AlternativeClass[Either[R, ?]] {
      override def or[A](f1: Either[R, A], f2: => Either[R, A]): Either[R, A] =
        f1 match {
          case Left(_)  => f2
          case Right(_) => f1
        }
    })
}
