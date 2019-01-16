package scalaz.parsers

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
}
