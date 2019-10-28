package org.spartanz.parserz

import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class CallsExampleSpec extends Specification {

  object Example {

    object MyParser extends ParsersModule {
      override type Input = List[Char]
    }

    import MyParser.Grammar._
    import MyParser._

    type G[A] = Grammar[Any, Nothing, String, A]

    case class Call(name: ::[Char], args: List[Call])

    val char: G[Char] = "char" @@ consume({
      case c :: cs => Right(cs -> c)
      case Nil     => Left("eoi")
    }, {
      case (cs, c) => Right(c :: cs)
    })

    val alpha: G[Char]  = char.filter("expected: alphabetical")(_.isLetter).tag("alpha")
    val comma: G[Char]  = char.filter("expected: comma")(_ == ',').tag("comma")
    val paren1: G[Char] = char.filter("expected: open paren")(_ == '(').tag("open paren")
    val paren2: G[Char] = char.filter("expected: close paren")(_ == ')').tag("close paren")

    val args: G[List[Call]] = "args" @@ ((call ~ (comma ~ call).rep) | succeed(Nil)).map({
      case Left((e1, en)) => e1 :: en.map(_._2)
      case Right(_)       => Nil
    }, {
      case Nil      => Right(Nil)
      case e1 :: en => Left((e1, en.map((',', _))))
    })

    lazy val call: G[Call] = "call" @@ delay {
      (alpha.rep1 ~ paren1 ~ args ~ paren2).map(
        { case (((name, _), exp), _) => Call(name, exp) },
        { case Call(name, exp)       => (((name, '('), exp), ')') }
      )
    }

    val parser: (Unit, Input) => (Unit, String \/ (Input, Call))  = MyParser.parser(call)
    val printer: (Unit, (Input, Call)) => (Unit, String \/ Input) = MyParser.printer(call)
    val description: List[String]                                 = MyParser.bnf(call)
  }

  import Example.Call

  private def parse(s: String) = Example.parser((), s.toList)._2.right.get._2
  private def print(c: Call)   = Example.printer((), (Nil, c))._2.map(_.reverse.mkString).right.get

  private def assert(s: String, c: Call): MatchResult[Any] = {
    val parsed  = parse(s)
    val printed = print(parsed)
    (parsed must_=== c).and(printed must_=== s)
  }

  "single" in {
    assert("a()", Call(::('a', Nil), Nil))
  }
  "with one arg" in {
    assert("a(bx())", Call(::('a', Nil), List(Call(::('b', List('x')), Nil))))
  }
  "with two args" in {
    assert(
      "a(bx(),cy(z()))",
      Call(::('a', Nil), List(Call(::('b', List('x')), Nil), Call(::('c', List('y')), List(Call(::('z', Nil), Nil)))))
    )
  }
  "description" in {
    Example.description.mkString("\n", "\n", "\n") must_===
      """
        |<alpha> ::= <char>
        |<open paren> ::= <char>
        |<comma> ::= <char>
        |<args> ::= (<call> List(<comma> <call>) | )
        |<close paren> ::= <char>
        |<call> ::= NEL(<alpha>) <open paren> <args> <close paren>
        |""".stripMargin
  }
}
