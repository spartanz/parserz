package org.spartanz.parserz.compare

import org.spartanz.parserz.\/
import org.spartanz.parserz.prototype.P2Module

object ParserzJsonTest {

  object Js {
    sealed trait Val extends Any {
      def value: Any
    }
    case class Str(value: String) extends AnyVal with Val
    case class Obj(value: (String, Val)*) extends AnyVal with Val
    case class Arr(value: Val*) extends AnyVal with Val
    case class Num(value: Double) extends AnyVal with Val
    case object False extends Val { def value: Any = false }
    case object True extends Val { def value: Any = true }
    case object Null extends Val { def value: Any = null }
  }


  object Parser extends P2Module {
    override type Input = List[Char]
    override type S     = Unit
    override type E     = String
  }

  import Parser.Grammar._
  import Parser.Tools._
  import Parser.Expr._
  import Parser._
  import Js._

  def char(c: Char): G[Char] = consume(
    cs => if (cs.nonEmpty && cs.head == c) Right((cs.tail, c)) else Left("expected: " + c),
    { case (cs, _) => Right(c :: cs) }
  )
  def token(t: List[Char]): G[List[Char]] = consume(
    cs => if (cs.startsWith(t)) Right((cs.drop(t.length), t)) else Left("expected: " + t),
    { case (cs, _) => Right(t reverse_::: cs) }
  )

  val dot: G[Char]      = char('.')
  val comma: G[Char]    = char(',')
  val colon: G[Char]    = char(':')
  val quote: G[Char]    = char('"')
  val bracket1: G[Char] = char('[')
  val bracket2: G[Char] = char(']')
  val brace1: G[Char]   = char('{')
  val brace2: G[Char]   = char('}')

  val spacing: G[Unit] = consumePure(
    cs => (cs.dropWhile(c => c == ' ' || c == '\n' || c == '\r'), ()),
    { case (cs, _) => ' ' :: cs }
  )

  val ch: G[Char] = consume(
    cs => if (cs.nonEmpty) Right((cs.tail, cs.head)) else Left("expected: char"),
    { case (cs, c) => Right(c :: cs) }
  )

  def chars(cond: Char => Boolean): G[List[Char]] = consumePure({
    cs =>
      val out = cs.takeWhile(cond)
      (cs.drop(out.length), out)
    }, {
      case (cs, cs1) => cs1 reverse_::: cs
    })

  val digits: G[List[Char]]     = chars(c => '0' <= c && c <= '9')
  val sign: G[Option[Char]]     = ch.filter("expected: +/-")(in('+', '-')).option
  val exponent: G[List[Char]]   = (ch.filter("expected: E")(in('e', 'E')) ~ sign, ('E', Some('+'))) ~> digits
  val fractional: G[List[Char]] = (dot, '.') ~> digits
  val integral: G[List[Char]]   = digits

  implicit val numEquiv: Equiv[(((Option[Char], List[Char]), List[Char]), List[Char]), Num] =
    Equiv.caseClass4(
      { case (s, l1, l2, l3) => Num((s.mkString + l1.mkString + l2.mkString + l3.mkString).toDouble) },
      { case Num(_) => ??? }
    )

  val num: G[Num] = toZ(sign ~ integral ~ fractional.orEmpty ~ exponent.orEmpty)

  val `null`: G[Null.type]   = token("null".toList).map( _ => Null,  _ => "null".toList)
  val `false`: G[False.type] = token("false".toList).map(_ => False, _ => "false".toList)
  val `true`: G[True.type]   = token("true".toList).map( _ => True,  _ => "true".toList)

  val string: G[String] = ((spacing ~ quote, ((), '"')) ~> chars(c => c != '\"' && c != '\\') <~ ('"', quote)).map(_.mkString, _.toList)
  val str: G[Str] = string.map(Str, "\"" + _.value + "\"")

  val arr: G[Arr] = ((bracket1, '[') ~> js.separated(comma).map(_.values, { _: List[Val] => ??? }) <~ (((), ']'), spacing ~ bracket2)).map(
    lst => Arr(lst : _*),
    arr => arr.value.toList
  )

  val field: G[(String, Val)] = (string <~ (':', colon)) ~ js

  val obj: G[Obj] = ((brace1, '{') ~> field.separated(comma).map(_.values, { _: List[(String, Val)] => ??? }) <~ (((), '}'), spacing ~ brace2)).map(
    lst => Obj(lst : _*),
    obj => obj.value.toList
  )

  // todo: smash ?
  lazy val js: G[Val] = delay {
    ((spacing, ()) ~> (obj | arr | str | `true` | `false` | `null` | num) <~ ((), spacing)).map({
      case Left(Left(Left(Left(Left(Left(v))))))  => v
      case Left(Left(Left(Left(Left(Right(v)))))) => v
      case Left(Left(Left(Left(Right(v)))))       => v
      case Left(Left(Left(Right(v))))             => v
      case Left(Left(Right(v)))                   => v
      case Left(Right(v))                         => v
      case Right(v)                               => v
    }, {
      case j: Obj => Left(Left(Left(Left(Left(Left(j))))))
      case j: Arr => Left(Left(Left(Left(Left(Right(j))))))
      case j: Str => Left(Left(Left(Left(Right(j)))))
      case True   => Left(Left(Left(Right(True))))
      case False  => Left(Left(Right(False)))
      case Null   => Left(Right(Null))
      case j: Num => Right(j)
    })
  }


  val parser: (S, Input) => (S, E \/ (Input, Val))  = Parser.parser[S, E, Val](js)


  def main(args: Array[String]): Unit = {
    // ((),Right((List(),Obj(List((firstName,Str(John)), (lastName,Str(Smith)), (age,Num(25.0)), (address,Obj(List((streetAddress,Str(21 2nd Street)), (city,Str(New York)), (state,Str(NY)), (postalCode,Num(10021.0))))), (phoneNumbers,Arr(List(Obj(List((type,Str(home)), (number,Str(212 555-1234)))), Obj(List((type,Str(fax)), (number,Str(646 555-4567))))))))))))
    println(parser((), value))

    val t1: Long = System.nanoTime()
    (1 to 1000000).foreach { _ =>
      parser((), value)
    }
    val t2: Long = System.nanoTime() - t1
    println(s"\n\n Execution time = ${(t2 / 1000).toString.reverse.grouped(3).map(_.reverse).toList.reverse.mkString(",")} μs")
  }

  //   100,000  in  3.3 sec
  // 1,000,000  in 24.9 sec

  val value: List[Char] =
    """{
      |  "firstName": "John",
      |  "lastName": "Smith",
      |  "age": 25,
      |  "address": {
      |      "streetAddress": "21 2nd Street",
      |      "city": "New York",
      |      "state": "NY",
      |      "postalCode": 10021
      |  },
      |  "phoneNumbers": [
      |      {
      |          "type": "home",
      |          "number": "212 555-1234"
      |      },
      |      {
      |          "type": "fax",
      |          "number": "646 555-4567"
      |      }
      |  ]
      |}""".stripMargin.toList
}
