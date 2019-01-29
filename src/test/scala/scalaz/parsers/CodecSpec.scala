package scalaz.parsers

import org.specs2.mutable.Specification
import scalaz.tc.Monad

class CodecSpec extends Specification {

  "Instantiate Parsing" >> {
    "given instances of applicative and category" in {
      import implicits.monadKleisliCategory
      import TCInstances.applicativeOption
      Parsing[Option, Option]() must_!= null
    }

    "given instances of monad" in {
      val M: Monad[Option] = implicitly
      Parsing[Option, Option](M, M) must_!= null
    }
  }
}
