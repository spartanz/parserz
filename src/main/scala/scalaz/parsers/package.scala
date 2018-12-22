package scalaz

import scalaz.tc.InstanceOf

package object parsers {

  type /\[A, B] = (A, B)
  type \/[A, B] = Either[A, B]

  type Alternative[F[_]]    = InstanceOf[AlternativeClass[F]]
  type ProductFunctor[F[_]] = InstanceOf[ProductFunctorClass[F]]
}
