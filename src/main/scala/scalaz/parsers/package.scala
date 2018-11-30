package scalaz

import scalaz.tc.InstanceOf

package object parsers {

  type Alternative[F[_]] = InstanceOf[AlternativeClass[F]]
}
