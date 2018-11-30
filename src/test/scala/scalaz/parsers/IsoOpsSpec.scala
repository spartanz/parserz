package scalaz.parsers

import org.specs2.mutable.Specification
import scalaz.Scalaz.Id
import scalaz.data.{ ~>, ∀ }
import scalaz.parsers.Combinators._

class IsoOpsSpec extends Specification {

  private type TIso[A, B] = Iso[Id, Id, A, B]

  private val iso = new TIso[Int, String] {
    override def to: UFV[Int, String]   = _.toString
    override def from: UGV[String, Int] = _.toInt
  }

  implicit private val id: Id ~> Id = ∀.mk[Id ~> Id].from(identity)

  "Transforming Iso" >> {
    "via reverse" in {
      iso.to(1) must_=== (~iso).from(1)
      iso.from("1") must_=== (~iso).to("1")
      iso.to(1) must_=== (~(~iso)).to(1)
    }
  }
}
