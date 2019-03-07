package scalaz.parsers

import scalaz.tc._

trait ApplicativeErrorClass[F[_], E] extends ApplicativeClass[F] {
  def raiseError[A](e: E): F[A]
  def handleError[A](fa: F[A])(f: E => F[A]): F[A]
}
