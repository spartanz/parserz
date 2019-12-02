package org.spartanz.parserz

import org.specs2.mutable.Specification

object MonadExampleSpec {

  sealed trait Header
  case class MyHeader(protocol: String) extends Header

  sealed trait Body
  case class TestBody(data: String) extends Body
  case class Pro1Body(data: String) extends Body
  case class Pro2Body(data: String) extends Body

  object Parser extends ParsersModule {
    override type Input = String
    implicit val headerEnumerable: Enumerable[Header] = new Enumerable[Header] {
      val min: Header = MyHeader("test")
      val max: Header = MyHeader("pro1")
      def range(minA: Header, maxA: Header): Iterable[Header] = List(
        minA,
        maxA
      )
    }
  }

  import Parser._
  import Parser.Expr._
  import Parser.Grammar._

  type S    = Int
  type E    = String
  type G[A] = Grammar[S, S, E, A]

  private val `-` = '-'

  val char: G[Char] = consumeStatefully(
    { case (s, i)      => if (i.nonEmpty) (s + 1, Right((i.drop(1), i.head))) else (s, Left("eoi")) },
    { case (s, (i, a)) => (s, Right(i + a.toString)) }
  )

  val eoi: G[Unit] = consumeStatefully(
    { case (s, i)      => if (i.isEmpty) (s, Right((i, ()))) else (s, Left("expected: eoi")) },
    { case (s, (i, _)) => (s, Right(i)) }
  )

  val divider: G[Char] = char.filter("expected: divider")(===(`-`))

  val protocol: G[String] = (char.filter("expected: alphabetical")(=!=(`-`)).rep <~ ((`-`, divider))) ∘ (_.mkString, _.toList)

  val header: G[Header] = protocol ∘ (MyHeader, { case MyHeader(p) => p })

  val testBody: G[TestBody] = (char.rep <~ (((), eoi))) ∘ (cs => TestBody(cs.mkString), _.data.toList)
  val pro2Body: G[Pro2Body] = (char.filter("expected 'B'")(===('B')).rep <~ (((), eoi))) ∘ (cs => Pro2Body(cs.mkString), _.data.toList)
  val pro1Body: G[Pro1Body] = (char.filter("expected 'A'")(===('A')).rep <~ (((), eoi))) ∘ (cs => Pro1Body(cs.mkString), _.data.toList)

  val asTest: G[Body] = testBody.mapPartial("expected: Test")({ case t => t }, { case t: TestBody => t })
  val asPro2: G[Body] = pro2Body.mapPartial("expected: Pro2")({ case t => t }, { case t: Pro2Body => t })
  val asPro1: G[Body] = pro1Body.mapPartial("expected: Pro1")({ case t => t }, { case t: Pro1Body => t })

  val body: G[Body] = header.flatMap {
    case MyHeader("test") => asTest
    case MyHeader("pro2") => asPro2
    case MyHeader("pro1") => asPro1
    case MyHeader(p)      => fail[E, Body](s"unsupported protocol '$p'")
  }

  val parserH: (S, Input) => (S, E \/ (Input, Header))  = Parser.parser[S, E, Header](header)
  val printerH: (S, (Input, Header)) => (S, E \/ Input) = Parser.printer[S, E, Header](header)

  val parserB: (S, Input) => (S, E \/ (Input, Body))  = Parser.parser[S, E, Body](body)
  val printerB: (S, (Input, Body)) => (S, E \/ Input) = Parser.printer[S, E, Body](body)
}

class MonadExampleSpec extends Specification {
  import MonadExampleSpec._

  private def parseHeader(s: String) = parserH(0, s)
  private def printHeader(h: Header) = printerH(0, ("", h))
  private def parsePacket(s: String) = parserB(0, s)
  private def printPacket(b: Body)   = printerB(0, ("", b))

  "empty" in {
    parseHeader("") must_=== 0 -> Left("eoi")
  }
  "not a good header" in {
    parseHeader("pro1") must_=== 4 -> Left("eoi")
  }
  "-> header" in {
    parseHeader("pro1-ABC") must_=== 6 -> Right("ABC" -> MyHeader("pro1"))
  }
  "<- header" in {
    printHeader(MyHeader("pro1")) must_=== 0 -> Right("pro1-")
  }

  "-> test packet" in {
    parsePacket("test-ABC") must_=== 9 -> Right("" -> TestBody("ABC"))
  }
  "<- test packet" in {
    printPacket(TestBody("ABC")) must_=== 0 -> Right("test-ABC")
  }

//  "-> unknown packet" in {
//    parsePacket("blah-?!?") must_=== 9 -> Right("" -> TestBody("ABC"))
//  }

  "-> wrong pro1 packet" in {
    parsePacket("pro1-ABC") must_=== 8 -> Left("expected: eoi")
  }
  "<- wrong pro1 packet" in {
    printPacket(Pro1Body("ABC")) must_=== 0 -> Left("expected: Test")
  }
  "-> correct pro1 packet" in {
    parsePacket("pro1-AAA") must_=== 9 -> Right("" -> Pro1Body("AAA"))
  }
  "<- correct pro1 packet" in {
    printPacket(Pro1Body("AAA")) must_=== 0 -> Right("pro1-AAA")
  }

//  "-> wrong pro2 packet" in {
//    parsePacket("pro2-ABC") must_=== 8 -> Left("expected: eoi")
//  }
//  "-> correct pro2 packet" in {
//    parsePacket("pro2-BBB") must_=== 9 -> Right("" -> Pro2Body("BBB"))
//  }
}
