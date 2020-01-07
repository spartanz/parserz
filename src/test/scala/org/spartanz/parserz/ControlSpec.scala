package org.spartanz.parserz

import org.specs2.mutable.Specification

object ControlSpec {

  object Parser extends ParsersModule {
    override type Input = String
  }

  import Parser._
  import Parser.Grammar._

  type S = Unit
  type E = String
  type G[A] = Grammar[Any, Nothing, E, A]

  val char: G[Char] = consumeOption("no input")(
    s => s.headOption.map(s.drop(1) -> _),
    { case (s, c) => Some(s + c.toString) }
  )

  val charOrEnd: G[Char] = consumePure(
    s => s.headOption.fold(s -> '$')(s.drop(1) -> _),
    { case (s, c) => s + (if (c == '$') "" else c.toString) }
  )

  val successful: G[List[Char]] = (succeed('-'), '-') ~> char.rep
  val failed: G[List[Char]] = (fail[E, Char]("on purpose"), '-') ~> char.rep

  def parse[A](g: G[A]): Input => E \/ (Input, A) = parser[S, E, A](g)((), _)._2
  def print[A](g: G[A]): A => E \/ Input          = a => printer[S, E, A](g)((), ("", a))._2
}

class ControlSpec extends Specification {
  import ControlSpec._

  "recovering to empty" should {
    "-> successful" in {
      parse(successful)("AB") must_=== Right(("", List('A', 'B')))
      parse(successful.orEmpty)("AB") must_=== Right(("", List('A', 'B')))
      parse(successful.recover(Nil))("AB") must_=== Right(("", List('A', 'B')))
    }
    "-> failed" in {
      parse(failed)("AB") must_=== Left("on purpose")
      parse(failed.orEmpty)("AB") must_=== Right(("AB", Nil))
      parse(failed.recover(Nil))("AB") must_=== Right(("AB", Nil))
    }
  }

  "consuming always yields" should {
    "->" in {
      parse(charOrEnd)("AB") must_=== Right(("B", 'A'))
      parse(charOrEnd)("") must_=== Right(("", '$'))
    }
    "<-" in {
      print(charOrEnd)('A') must_=== Right("A")
      print(charOrEnd)('$') must_=== Right("")
    }
  }
}
