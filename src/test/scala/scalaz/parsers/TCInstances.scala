package scalaz.parsers

import scalaz.{ ApplicativeError, Monad, MonadError, ~> }

object TCInstances {

  implicit val optionApplicativeError: ApplicativeError[Option, Unit] =
    TCInstances0.optionMonadError

  implicit def naturalTransformationLoop[F[_]]: F ~> F =
    new ~>[F, F] {
      override def apply[A](fa: F[A]): F[A] = fa
    }
}

object TCInstances0 {

  val optionMonadError: MonadError[Option, Unit] = {
    val F: Monad[Option] = scalaz.std.option.optionInstance
    new MonadError[Option, Unit] {
      override def raiseError[A](e: Unit): Option[A]                              = None
      override def handleError[A](fa: Option[A])(f: Unit => Option[A]): Option[A] = fa.orElse(f(()))
      override def point[A](a: => A): Option[A]                                   = F.point(a)
      override def bind[A, B](fa: Option[A])(f: A => Option[B]): Option[B]        = F.bind(fa)(f)
    }
  }
}
