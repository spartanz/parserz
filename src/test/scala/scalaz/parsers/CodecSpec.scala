package scalaz.parsers

import org.specs2.mutable.Specification
import scalaz.MonadError
import scalaz.parsers.tc.Category
import scalaz.std.option._

class CodecSpec extends Specification {

  "Instantiate Parsing" >> {
    "given instances of applicative and category" in {
      import Category._
      import TCInstances.optionApplicativeError
      Parsing[Option, Option, Unit]()
      success
    }

    "given instances of monad" in {
      val M: MonadError[Option, Unit] = TCInstances0.optionMonadError
      Parsing[Option, Option, Unit](M, M)
      success
    }
  }
}
