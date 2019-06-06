package scalaz.parsers

import org.specs2.mutable.Specification
import scalaz.Show
import scalaz.parsers.tc.Category
import scalaz.std.either._
import scalaz.std.string._
import scalaz.std.anyVal.char

class IPAddressSpec extends Specification {

  object Syntax {
    case class Subnet(value: Int)
    case class IPAddress(a: Subnet, b: Subnet, c: Subnet, d: Subnet)

  }

  object Example {
    import TCInstances._
    import Syntax._
    import Category._

    val parsing: Parsing[Either[String, ?], Either[String, ?], String] = Parsing()

    import parsing.Equiv
    import parsing.Equiv._

    val subnetInt: Equiv[Int, Subnet] =
      liftPartial("Expected: Subnet value <= 255")(
        { case i if i <= 255 => Subnet(i) },
        { case s             => s.value }
      )

    type Codec[A] = parsing.Codec[String, A]

    val char: Codec[Char] = parsing.Codec[String, Char](
      liftFG(
        s =>
          s.headOption
            .fold[Either[String, Char]](Left("Empty input"))(Right(_))
            .map(s.drop(1) -> _),
        { case (s, c) => Right(s + c) }
      )
    )

    val subnet: Codec[Subnet] = char.n(3) ∘ dotCodec.one

    /***
   *
   *      subnetA . subNetB . subNetC . subNetD
   *
   *
   *      subnet (`.` subnet ).
   *
   *
   *    case class Separated1[S, A](head: A, tail: Option[S /\ Separated1[S, A]])
   *    Cofree[Option, A] ??
   *
   *
   *    subnet many ... ensure len < 4
   *
   */

  }

}
