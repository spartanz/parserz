package org.spartanz.parserz

import org.specs2.mutable.Specification

class FunExampleSpec extends Specification {

  object Stateless {

    object Parser extends ParsersModule {
      override type Input = String
    }

    type S = Unit
    type E = String

    import Parser._
    import Parser.Grammar._

    val good: Grammar[Any, Nothing, Nothing, String] = succeed("🎁")
    val bad: Grammar[Any, Nothing, E, String]        = fail("🚫")

    def parser[A](g: Grammar[Any, Nothing, E, A]): Input => E \/ (Input, A)    = Parser.parser[S, E, A](g)((), _)._2
    def printer[A](g: Grammar[Any, Nothing, E, A]): ((Input, A)) => E \/ Input = Parser.printer[S, E, A](g)((), _)._2
  }

  "Stateless parser" should {
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
  }

  object Stateful {

    object Parser extends ParsersModule {
      override type Input = String
    }

    type S = Int
    type E = String

    import Parser._
    import Parser.Grammar._

    val neutral: Grammar[Any, Nothing, E, Char] = consume0(
      s => s.headOption.map(s.drop(1) -> _).map(Right(_)).getOrElse(Left("empty")),
      { case (s, c) => Right(s + c.toString) }
    )

    val effectful: Grammar[S, S, E, Char] = consume(
      { case (si, s)      => si + 1 -> s.headOption.map(s.drop(1) -> _).map(Right(_)).getOrElse(Left("empty")) },
      { case (si, (s, c)) => si - 1 -> Right(s + c.toString) }
    )

    def parser[A](g: Grammar[S, S, E, A]): (S, Input) => (S, E \/ (Input, A))  = Parser.parser[S, E, A](g)
    def printer[A](g: Grammar[S, S, E, A]): (S, (Input, A)) => (S, E \/ Input) = Parser.printer[S, E, A](g)
  }

  "Stateful parser" should {
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

    "-> consume value (with more state change)" in {
      parser(effectful ~ neutral ~ effectful)(0, "abc") must_=== ((2, Right(("", (('a', 'b'), 'c')))))
    }
    "<- produce value (with more state change)" in {
      printer(effectful ~ neutral ~ effectful)(0, ("", (('a', 'b'), 'c'))) must_=== ((-2, Right("abc")))
    }
  }
}
