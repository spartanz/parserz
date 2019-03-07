package scalaz.parsers

import org.specs2.mutable.Specification

class CodecSpec extends Specification {

  "Instantiate Parsing" >> {
    "given instances of applicative and category" in {
      import implicits.monadKleisliCategory
      import implicits.monadErrorApplicativeError
      import TCInstances.applicativeErrorOption
      Parsing[Option, Option, Unit]()
      success
    }

    "given instances of monad" in {
      val M: MonadError[Option, Unit] = TCInstances0.monadErrorOption
      Parsing[Option, Option, Unit](M, M)
      success
    }
  }
}
