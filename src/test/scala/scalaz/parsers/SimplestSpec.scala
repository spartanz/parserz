package scalaz.parsers

import org.specs2.mutable.Specification
import scalaz.parsers.Simplest.Parsers
import scalaz.parsers.Simplest.Syntax.Literal

class SimplestSpec extends Specification {

  "Simplest parser" should {
    "parse a digit into a literal" in {
      Parsers.expression("5".toList) must_=== Right(Nil -> Literal(5))
    }

    "try to parse two digits and finish after first (because it's indeed simplest)" in {
      Parsers.expression("55".toList) must_=== Right(List('5') -> Literal(5))
    }

    "try to parse a letter and indicate failure" in {
      Parsers.expression("A".toList) must_=== Left(())
    }
  }
}
