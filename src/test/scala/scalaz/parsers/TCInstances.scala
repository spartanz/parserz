package scalaz.parsers

import scalaz.data.{ ~>, ∀ }
import scalaz.tc.FoldableClass.{ DeriveFoldMap, DeriveToList }
import scalaz.tc.{ Applicative, Foldable, FoldableClass, instanceOf }

object TCInstances {
  import scalaz.Scalaz.monadApplicative

  implicit val applicativeOption: Applicative[Option] =
    monadApplicative[Option](implicitly)

  implicit val foldableOption: Foldable[Option] = instanceOf(
    new FoldableClass[Option] with DeriveFoldMap[Option] with DeriveToList[Option] {
      override def foldRight[A, B](fa: Option[A], z: => B)(f: (A, => B) => B): B =
        fa.fold(z)(f(_, z))
      override def foldLeft[A, B](fa: Option[A], z: B)(f: (B, A) => B): B =
        fa.fold(z)(f(z, _))
    }
  )

  implicit val OptionToOption: Option ~> Option =
    ∀.mk[Option ~> Option].from(identity)
}
