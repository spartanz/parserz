package scalaz.parsers

import org.specs2.mutable.Specification
import scalaz.parsers.Simplest.{ Parser, Printer }
import scalaz.parsers.Simplest.Syntax._

class SimplestSpec extends Specification {

  "Simplest parser" should {
    "not parse empty input" in {
      parse("") must_=== Left(())
    }

    "parse a digit into a literal" in {
      parse("5") must_=== Right(Nil -> Constant(5))
    }

    "try to parse two digits and finish after first (because it's indeed simplest)" in {
      parse("55") must_=== Right(List('5') -> Constant(5))
    }

    "not parse a letter and indicate failure" in {
      parse("A") must_=== Left(())
    }

    "not parse '+' by itself" in {
      parse("+") must_=== Left(())
    }

    "parse sum of 2 numbers" in {
      parse("5+6") must_=== Right(Nil -> Sum(Constant(5), Constant(6)))
    }

    "parse sum of 3 numbers" in {
      parse("5+6+7") must_=== Right(Nil -> Sum(Sum(Constant(5), Constant(6)), Constant(7)))
    }
  }

  import Simplest.ScalazInstances._

  private val parser = Simplest.grammar[Parser](Parser)
  private val print  = Simplest.grammar[Printer](Printer)

  private def parse(s: String) = parser(s.toList)

  "Simplest printer" should {
    "print a constant" in {
      print(Constant(1)) must_=== "1"
    }
    "not print a multi-digit number (not yet supported)" in {
      print(Constant(23)) must_=== "2"
    }
    "print a sum" in {
      print(Sum(Constant(1), Constant(2))) must_=== "1+2"
      print(Sum(Sum(Constant(1), Constant(2)), Constant(3))) must_=== "1+2+3"
      print(Sum(Sum(Sum(Constant(1), Constant(2)), Constant(3)), Constant(4))) must_=== "1+2+3+4"
    }
    "not print an incorrectly composed expression" in {
      print(Sum(Constant(1), Sum(Constant(2), Constant(3)))) must_=== "1+"
      print(Sum(Sum(Constant(1), Constant(2)), Sum(Constant(3), Constant(4)))) must_=== "1+2+"
    }
  }
}
