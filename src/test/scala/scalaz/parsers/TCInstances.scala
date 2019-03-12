package scalaz.parsers

import scalaz.data.{ ~>, ∀ }
import scalaz.parsers.implicits.monadErrorApplicativeError
import scalaz.tc.BindClass.DeriveFlatMap
import scalaz.tc.FoldableClass.{ DeriveFoldMap, DeriveToList }
import scalaz.tc._

object TCInstances {
  import scalaz.Scalaz.monadApplicative

  implicit val applicativeOption: Applicative[Option] =
    monadApplicative[Option](implicitly)

  implicit val applicativeErrorOption: ApplicativeError[Option, Unit] =
    monadErrorApplicativeError[Option, Unit](TCInstances0.monadErrorOption)

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

object TCInstances0 {

  val monadErrorOption: MonadError[Option, Unit] = {
    val F: Monad[Option] = implicitly
    instanceOf(new MonadErrorClass[Option, Unit] with DeriveFlatMap[Option] {
      override def raiseError[A](e: Unit): Option[A] =
        None
      override def handleError[A](fa: Option[A])(f: Unit => Option[A]): Option[A] =
        fa.orElse(f(()))

      override def pure[A](a: A): Option[A]                              = F.pure(a)
      override def ap[A, B](fa: Option[A])(f: Option[A => B]): Option[B] = F.ap(fa)(f)
      override def map[A, B](ma: Option[A])(f: A => B): Option[B]        = F.map(ma)(f)
      override def flatten[A](ma: Option[Option[A]]): Option[A]          = F.flatten(ma)
    })
  }
}
