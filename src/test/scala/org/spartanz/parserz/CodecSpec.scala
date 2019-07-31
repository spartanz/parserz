package org.spartanz.parserz

import org.specs2.mutable.Specification
import scalaz.MonadError
import org.spartanz.parserz.tc.Category
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

  "Codec.delay" should {
    import Category._
    import TCInstances.optionApplicativeError
    val env = Parsing[Option, Option, Unit]()
    import env._

    "add trampoline to Codec.equiv usage" in {
      lazy val c1: Codec[String, String] = c2
      lazy val c2: Codec[String, String] = c1
      // accessing `c1` via `delay` does not result in stack overflow
      Codec.delay(c1)
      success
    }
  }
}
