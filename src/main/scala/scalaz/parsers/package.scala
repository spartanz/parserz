package scalaz

import scalaz.tc.InstanceOf

package object parsers {

  type /\[A, B] = (A, B)
  type \/[A, B] = Either[A, B]

  type Alternative[F[_]]         = InstanceOf[AlternativeClass[F]]
  type ProductFunctor[F[_]]      = InstanceOf[ProductFunctorClass[F]]
  type ApplicativeError[F[_], E] = InstanceOf[ApplicativeErrorClass[F, E]]
  type MonadError[F[_], E]       = InstanceOf[MonadErrorClass[F, E]]

  type Transform[=>:[_, _]] = InstanceOf[TransformClass[=>:]]
}
