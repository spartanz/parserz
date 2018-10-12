package scalaz.parsers

import org.specs2.Specification

class TestSpec extends Specification {

  def is = s2"""
      tested ${Test.tested() must_== 3}
      """
}
