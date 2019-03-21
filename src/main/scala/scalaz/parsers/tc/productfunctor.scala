package scalaz.parsers.tc

import scalaz.parsers./\

trait ProductFunctorClass[F[_]] {
  def and[A, B](fa: F[A], fb: F[B]): F[A /\ B]
}
