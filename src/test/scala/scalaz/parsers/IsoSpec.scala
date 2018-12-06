package scalaz.parsers

import org.specs2.mutable.Specification
import scalaz.tc._

class IsoSpec extends Specification {

  private type Id[A]      = A
  private type TFun[A, B] = A => Id[B]
  private type TIso[A, B] = Iso[Id, Id, A, B]
  private object TIso extends ProductIso[Id, Id]

  implicit private val funCategory: Category[TFun] = instanceOf(
    new CategoryClass[TFun] {
      def id[A]: TFun[A, A]                                          = identity
      def compose[A, B, C](f: TFun[B, C], g: TFun[A, B]): TFun[A, C] = g.andThen(f)
    }
  )

  implicit private val idApplicative: Applicative[Id] = instanceOf(
    new ApplicativeClass[Id] {
      def pure[A](a: A): Id[A]                      = a
      def ap[A, B](fa: Id[A])(f: Id[A => B]): Id[B] = f(fa)
      def map[A, B](fa: Id[A])(f: A => B): Id[B]    = f(fa)
    }
  )

  private def ignoreL[A](a: A): TIso[Unit, A] = new TIso[Unit, A] {
    def to: UFV[Unit, A]   = _ => a
    def from: UGV[A, Unit] = _ => ()
  }

  private def ignoreR[A](a: A): TIso[A, Unit] = new TIso[A, Unit] {
    def to: UFV[A, Unit]   = _ => ()
    def from: UGV[Unit, A] = _ => a
  }

  private def verify[A, B](iso: TIso[A, B], a: A, b: B) =
    (iso.to(a) must_=== b)
      .and(iso.from(b) must_=== a)
      .and(iso.from(iso.to(a)) must_=== a)

  import TIso._

  "Constructing Iso" >> {
    "via unit" in {
      verify(unitL[Int], 1, ((), 1))
      verify(unitR[Int], 2, (2, ()))
    }
    "via associate" in {
      verify(
        associate[Int, Long, String],
        (1, (2L, "s")),
        ((1, 2L), "s")
      )
    }
    "via flatten" in {
      verify(
        flatten[Int, Long, String],
        (1, (2L, "s")),
        (1, 2L, "s")
      )
    }
  }

  "Transforming Iso" >> {
    "via unit" in {
      val iso1: TIso[Unit, Int] = ignoreL(5)

      verify(iso1 >>> unitL, (), ((), 5))
      verify(iso1 >>> unitR, (), (5, ()))
    }

    "via associate" in {
      val iso1: TIso[Unit, (Int, (Long, String))] = ignoreL((1, (2L, "s")))
      val iso2: TIso[Unit, ((Int, Long), String)] = iso1 >>> associate

      verify(iso2, (), ((1, 2L), "s"))

      val iso3: TIso[((Int, Long), String), Unit] = ignoreR(((1, 2L), "s"))
      val iso4: TIso[(Int, (Long, String)), Unit] = iso3 <<< associate

      verify(iso4, (1, (2L, "s")), ())
    }

    "via flatten" in {
      val iso1: TIso[Unit, (Int, (Long, String))] = ignoreL((1, (2L, "s")))
      val iso2: TIso[Unit, (Int, Long, String)]   = iso1 >>> flatten

      verify(iso2, (), (1, 2L, "s"))

      val iso3: TIso[(Int, Long, String), Unit]   = ignoreR((1, 2L, "s"))
      val iso4: TIso[(Int, (Long, String)), Unit] = iso3 <<< flatten

      verify(iso4, (1, (2L, "s")), ())
    }
  }
}
