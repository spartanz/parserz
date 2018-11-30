package scalaz.parsers

import org.specs2.mutable.Specification
import scalaz.parsers.Simplest.Parsers
import scalaz.parsers.Simplest.Syntax.{ Composition, Number }

class SimplestSpec extends Specification {

  "Simplest parser" should {
    "parse a digit into a literal" in {
      parse("5") must_=== Right(Nil -> Number(5))
    }

    "try to parse two digits and finish after first (because it's indeed simplest)" in {
      parse("55") must_=== Right(List('5') -> Number(5))
    }

    "not parse a letter and indicate failure" in {
      parse("A") must_=== Left(())
    }

    "not parse '+' by itself" in {
      parse("+") must_=== Left(())
    }

    "parse sum as expression" in {
      parse("5+5") must_=== Right(Nil -> Composition(Number(5), Number(5)))
    }
  }

  private def parse(s: String) =
    Parsers.expression(s.toList)
}
