package scalaz.parsers

trait AlternativeClass[F[_]] {
  def or[A](f1: F[A], f2: => F[A]): F[A]
}
