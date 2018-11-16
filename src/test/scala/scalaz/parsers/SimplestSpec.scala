package scalaz.parsers

import org.specs2.mutable.Specification
import scalaz.parsers.Convoluted.Parsers
import scalaz.parsers.Convoluted.Syntax.Number

class SimplestSpec extends Specification {

  "Simplest parser" should {
    "parse a digit into a literal" in {
      Parsers.expression("5".toList) must_=== Right(Nil -> Number(5))
    }

    "try to parse two digits and finish after first (because it's indeed simplest)" in {
      Parsers.expression("55".toList) must_=== Right(List('5') -> Number(5))
    }

    "try to parse a letter and indicate failure" in {
      Parsers.expression("A".toList) must_=== Left(())
    }
  }
}
