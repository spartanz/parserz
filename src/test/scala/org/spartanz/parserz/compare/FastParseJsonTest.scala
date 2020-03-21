package org.spartanz.parserz.compare

object FastParseJsonTest {

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

  import fastparse._
  import NoWhitespace._
  def stringChars(c: Char): Boolean = c != '\"' && c != '\\'

  def space[_: P]: P[Unit]      = P( CharsWhileIn(" \r\n", 0) )
  def digits[_: P]: P[Unit]     = P( CharsWhileIn("0-9") )
  def exponent[_: P]: P[Unit]   = P( CharIn("eE") ~ CharIn("+\\-").? ~ digits )
  def fractional[_: P]: P[Unit] = P( "." ~ digits )
  def integral[_: P]: P[Unit]   = P( "0" | CharIn("1-9")  ~ digits.? )

  def number[_: P]: P[Js.Num] = P(  CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.? ).!.map(
    x => Js.Num(x.toDouble)
  )

  def `null`[_: P]: P[Js.Null.type]   = P( "null" ).map(_ => Js.Null)
  def `false`[_: P]: P[Js.False.type] = P( "false" ).map(_ => Js.False)
  def `true`[_: P]: P[Js.True.type]   = P( "true" ).map(_ => Js.True)

  def hexDigit[_: P]: P[Unit]      = P( CharIn("0-9a-fA-F") )
  def unicodeEscape[_: P]: P[Unit] = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
  def escape[_: P]: P[Unit]        = P( "\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape) )

  def strChars[_: P]: P[Unit] = P( CharsWhile(stringChars) )
  def string[_: P]: P[Js.Str] = P( space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Js.Str)

  def array[_: P]: P[Js.Arr] =
    P( "[" ~/ jsonExpr.rep(sep=","./) ~ space ~ "]").map(Js.Arr(_:_*))

  def pair[_: P]: P[(String, Js.Val)] = P( string.map(_.value) ~/ ":" ~/ jsonExpr )

  def obj[_: P]: P[Js.Obj] =
    P( "{" ~/ pair.rep(sep=","./) ~ space ~ "}").map(Js.Obj(_:_*))

  def jsonExpr[_: P]: P[Js.Val] = P(
    space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space
  )


  def main(args: Array[String]): Unit = {
    println(parse(value, jsonExpr(_)))

    val t1: Long = System.nanoTime()
    (1 to 1000000).foreach { _ =>
      parse(value, jsonExpr(_))
    }
    val t2: Long = System.nanoTime() - t1
    println(s"\n\n Execution time = ${(t2 / 1000).toString.reverse.grouped(3).map(_.reverse).toList.reverse.mkString(",")} μs")
  }

  //   100,000  in  0.7 sec
  // 1,000,000  in  4.1 sec

  val value: String =
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
      |}""".stripMargin
}
