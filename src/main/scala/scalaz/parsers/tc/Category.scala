package scalaz.parsers.tc

import scalaz.Monad

trait Category[=>:[_, _]] {
  def id[A]: A =>: A
  def compose[A, B, C](f: B =>: C, g: A =>: B): A =>: C
}

object Category {

  implicit def kleisliCategory[F[_]](implicit F: Monad[F]): Category[λ[(α, β) => α => F[β]]] =
    new Category[λ[(α, β) => α => F[β]]] {
      override def id[A]: A => F[A] =
        F.pure(_)
      override def compose[A, B, C](f: B => F[C], g: A => F[B]): A => F[C] =
        a => F.bind(g(a))(f)
    }

  trait CategorySyntax {
    implicit final class ToCategoryOps[=>:[_, _], B, C](self: B =>: C) {
      def ∘ [A](f: A =>: B)(implicit C: Category[=>:]): A =>: C = C.compose(self, f)
    }
  }
}
