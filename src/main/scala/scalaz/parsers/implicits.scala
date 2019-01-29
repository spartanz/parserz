package scalaz.parsers

import scalaz.tc.{ Category, CategoryClass, Monad, instanceOf }

object implicits {

  implicit def monadKleisliCategory[F[_]](implicit F: Monad[F]): Category[λ[(α, β) => α => F[β]]] =
    instanceOf(
      new CategoryClass[λ[(α, β) => α => F[β]]] {
        override def id[A]: A => F[A] =
          F.pure
        override def compose[A, B, C](f: B => F[C], g: A => F[B]): A => F[C] =
          a => F.flatMap(g(a))(f)
      }
    )
}
