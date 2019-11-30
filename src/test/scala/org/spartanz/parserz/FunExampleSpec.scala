package org.spartanz.parserz

import org.specs2.mutable.Specification

object FunExampleSpec {

  object Stateless {

    object Parser extends ParsersModule {
      override type Input = String
    }

    type S = Unit
    type E = String

    import Parser._
    import Parser.Expr._
    import Parser.Grammar._

    val good: Grammar[Any, Nothing, Nothing, String] = succeed("🎁")
    val bad: Grammar[Any, Nothing, E, String]        = fail("🚫")

    val badFiltered: Grammar[Any, Nothing, E, String]  = bad.filter("not good")(===("✅")).tag("not suitable for bad")
    val badConfirmed: Grammar[Any, Nothing, E, String] = bad.filter("not good")(=!=("✅")).tag("suitable for bad")

    def parser[A](g: Grammar[Any, Nothing, E, A]): Input => E \/ (Input, A)    = Parser.parser[S, E, A](g)((), _)._2
    def printer[A](g: Grammar[Any, Nothing, E, A]): ((Input, A)) => E \/ Input = Parser.printer[S, E, A](g)((), _)._2
    def bnf[A](g: Grammar[Any, Nothing, E, A]): String                         = Parser.bnf(g).mkString("\n", "\n", "\n")
  }

  object Stateful {

    object Parser extends ParsersModule {
      override type Input = String
    }

    type S = Int
    type E = String

    import Parser._
    import Parser.Grammar._

    val neutral: Grammar[Any, Nothing, E, Char] = consume(
      s => s.headOption.map(s.drop(1) -> _).map(Right(_)).getOrElse(Left("empty")),
      { case (s, c) => Right(s + c.toString) }
    )

    val bad: Grammar[S, S, E, String] = fail(s => (s + 1, "🚫🚫"))

    val effectful: Grammar[S, S, E, Char] = consumeStatefully(
      { case (si, s)      => si + 1 -> s.headOption.map(s.drop(1) -> _).map(Right(_)).getOrElse(Left("empty")) },
      { case (si, (s, c)) => si - 1 -> Right(s + c.toString) }
    )

    def parser[A](g: Grammar[S, S, E, A]): (S, Input) => (S, E \/ (Input, A))  = Parser.parser[S, E, A](g)
    def printer[A](g: Grammar[S, S, E, A]): (S, (Input, A)) => (S, E \/ Input) = Parser.printer[S, E, A](g)
  }
}

class FunExampleSpec extends Specification {

  "Stateless parser" should {
    import FunExampleSpec._
    import Stateless._

    "-> generate value" in {
      parser(good)("abc") must_=== Right(("abc", "🎁"))
    }
    "<- ignore value" in {
      printer(good)("abc" -> "🎁") must_=== Right("abc")
    }

    "-> generate error" in {
      parser(bad)("abc") must_=== Left("🚫")
    }
    "<- generate error" in {
      printer(bad)("abc" -> "🎁") must_=== Left("🚫")
    }

    "-> filter generated error" in {
      parser(badFiltered)("abc") must_=== Left("🚫")
    }
    "<- filter generated error" in {
      printer(badFiltered)("abc" -> "🎁") must_=== Left("not good")
    }
    "!! filter generated error" in {
      bnf(badFiltered) must_===
        """
          |<not suitable for bad> ::= "✅"
          |""".stripMargin
    }

    "-> confirm generated error" in {
      parser(badConfirmed)("abc") must_=== Left("🚫")
    }
    "<- confirm generated error" in {
      printer(badConfirmed)("abc" -> "🎁") must_=== Left("🚫")
    }
    "!! confirm generated error" in {
      bnf(badConfirmed) must_===
        """
          |<suitable for bad> ::= - "✅"
          |""".stripMargin
    }
  }

  "Stateful parser" should {
    import FunExampleSpec._
    import Stateful._

    "-> consume value (no state change)" in {
      parser(neutral)(0, "abc") must_=== ((0, Right(("bc", 'a'))))
    }
    "<- produce value (no state change)" in {
      printer(neutral)(0, ("", 'a')) must_=== ((0, Right("a")))
    }

    "-> consume value (with state change)" in {
      parser(effectful)(0, "abc") must_=== ((1, Right(("bc", 'a'))))
    }
    "<- produce value (with state change)" in {
      printer(effectful)(0, ("", 'a')) must_=== ((-1, Right("a")))
    }

    "-> fail to consume value (with state change)" in {
      parser(effectful)(0, "") must_=== ((1, Left("empty")))
    }

    "-> always fail (with state change)" in {
      parser(bad)(0, "") must_=== ((1, Left("🚫🚫")))
    }

    "-> consume value (with more state change)" in {
      parser(effectful ~ neutral ~ effectful)(0, "abc") must_=== ((2, Right(("", (('a', 'b'), 'c')))))
    }
    "<- produce value (with more state change)" in {
      printer(effectful ~ neutral ~ effectful)(0, ("", (('a', 'b'), 'c'))) must_=== ((-2, Right("abc")))
    }
  }
}
