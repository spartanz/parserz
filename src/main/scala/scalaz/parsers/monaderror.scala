package scalaz.parsers

import scalaz.tc.BindClass.DeriveFlatMap
import scalaz.tc._

trait MonadErrorClass[F[_], E] extends ApplicativeErrorClass[F, E] with MonadClass[F] {}

object MonadErrorClass {

  implicit def eitherMonadError[E](
    implicit F: Monad[Either[E, ?]]
  ): MonadError[Either[E, ?], E] =
    instanceOf(new MonadErrorClass[Either[E, ?], E] with DeriveFlatMap[Either[E, ?]] {
      override def raiseError[A](e: E): Either[E, A] =
        Left(e)
      override def handleError[A](fa: Either[E, A])(f: E => Either[E, A]): Either[E, A] =
        fa match {
          case Left(e)      => f(e)
          case r @ Right(_) => r
        }

      override def pure[A](a: A): Either[E, A]                                    = F.pure(a)
      override def ap[A, B](fa: Either[E, A])(f: Either[E, A => B]): Either[E, B] = F.ap(fa)(f)
      override def map[A, B](ma: Either[E, A])(f: A => B): Either[E, B]           = F.map(ma)(f)
      override def flatten[A](ma: Either[E, Either[E, A]]): Either[E, A]          = F.flatten(ma)
    })
}
