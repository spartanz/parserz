package scalaz.parsers

import org.specs2.mutable.Specification
import scalaz.data.{ ~>, ∀ }
import scalaz.tc.FoldableClass.{ DeriveFoldMap, DeriveToList }
import scalaz.tc._

class IsoSpec extends Specification {

  private type TFun[A, B] = A => Option[B]
  private type TIso[A, B] = Iso[Option, Option, A, B]
  private object TIso extends IsoClass[Option, Option]

  private object Instances {
    import scalaz.Scalaz.monadApplicative

    implicit val fun1Category: Category[TFun] = instanceOf(
      new CategoryClass[TFun] {
        def id[A]: TFun[A, A]                                          = Option.apply
        def compose[A, B, C](f: TFun[B, C], g: TFun[A, B]): TFun[A, C] = g(_).flatMap(f)
      }
    )

    implicit val applicativeOption: Applicative[Option] =
      monadApplicative[Option](implicitly)

    implicit val foldableOption: Foldable[Option] = instanceOf(
      new FoldableClass[Option] with DeriveFoldMap[Option] with DeriveToList[Option] {
        override def foldRight[A, B](fa: Option[A], z: => B)(f: (A, => B) => B): B =
          fa.fold(z)(f(_, z))
        override def foldLeft[A, B](fa: Option[A], z: B)(f: (B, A) => B): B =
          fa.fold(z)(f(z, _))
      }
    )

    implicit val OptionToOption: Option ~> Option =
      ∀.mk[Option ~> Option].from(identity)
  }

  import Instances._

  private def verify[A, B](iso: TIso[A, B], a: A, b: B) =
    (iso.to(a).get must_=== b)
      .and(iso.from(b).get must_=== a)
      .and(iso.from(iso.to(a).get).get must_=== a)

  "Constructors" >> {
    import TIso.Product._
    import TIso._

    "id" in {
      verify(id[Int], 2, 2)
    }

    "lift" in {
      verify(lift[Int, Int](_ + 1, _ - 1), 3, 4)
      verify(liftF[Int, Int](a => Some(a + 1), b => Some(b - 1)), 3, 4)
    }

    "unit" in {
      verify(unitL[Int], 1, ((), 1))
      verify(unitR[Int], 2, (2, ()))
    }

    "commute" in {
      verify(commute[Int, String], (1, "s"), ("s", 1))
    }

    "associate" in {
      verify(associate[Int, Long, String], (1, (2L, "s")), ((1, 2L), "s"))
    }

    "flatten" in {
      verify(flatten[Int, Long, String], (1, (2L, "s")), (1, 2L, "s"))
    }
  }

  "Operations" >> {
    import TIso._
    val iso1: TIso[Int, Int] = lift(_ + 1, _ - 1)
    val iso2: TIso[Int, Int] = lift(_ + 2, _ - 2)

    "conjunction" >> {
      verify(iso1 /\ iso2, (0, 0), (1, 2))
    }

    "disjunction" >> {
      verify(iso1 \/ iso2, Left(0), Left(1))
      verify(iso1 \/ iso2, Right(0), Right(2))
    }

    "reverse" >> {
      verify(iso1, 0, 1)
      verify(~iso1, 1, 0)
    }

    "chain" >> {
      verify(iso1 >>> iso2, 0, 3)
      verify(iso1 <<< iso2, 0, 3)
      verify(~iso1 >>> iso2, 0, 1)
      verify(~iso1 <<< iso2, 0, 1)
      verify(~iso1 >>> ~iso2, 0, -3)
      verify(~iso1 <<< ~iso2, 0, -3)
    }
  }

  "Combinators" >> {
    import TIso.Product._
    import TIso._

    "create" in {
      verify(create(5), (), 5)
    }

    "ignore" in {
      verify(ignore(5), 5, ())
    }

    "id" in {
      val iso1: TIso[Unit, Int] = create(5)
      val iso2: TIso[Int, Unit] = ignore(5)
      verify(iso1 >>> id[Int], (), 5)
      verify(iso2 <<< id[Int], 5, ())
    }

    "lift" in {
      val iso1: TIso[Unit, Int] = create(5)
      verify(iso1 >>> lift[Int, Int](_ + 1, _ - 1), (), 6)
      verify(iso1 >>> liftF[Int, Int](a => Some(a + 1), b => Some(b - 1)), (), 6)
    }

    "unit" in {
      val iso1: TIso[Unit, Int] = create(5)
      verify(iso1 >>> unitL, (), ((), 5))
      verify(iso1 >>> unitR, (), (5, ()))
    }

    "commute" in {
      val iso1: TIso[Unit, (Int, String)] = create((1, "s"))
      val iso2: TIso[Unit, (String, Int)] = iso1 >>> commute
      verify(iso2, (), ("s", 1))

      val iso3: TIso[(Int, String), Unit] = ignore((1, "s"))
      val iso4: TIso[(String, Int), Unit] = iso3 <<< commute
      verify(iso4, ("s", 1), ())
    }

    "associate" in {
      val iso1: TIso[Unit, (Int, (Long, String))] = create((1, (2L, "s")))
      val iso2: TIso[Unit, ((Int, Long), String)] = iso1 >>> associate
      verify(iso2, (), ((1, 2L), "s"))

      val iso3: TIso[((Int, Long), String), Unit] = ignore(((1, 2L), "s"))
      val iso4: TIso[(Int, (Long, String)), Unit] = iso3 <<< associate
      verify(iso4, (1, (2L, "s")), ())
    }

    "flatten" in {
      val iso1: TIso[Unit, (Int, (Long, String))] = create((1, (2L, "s")))
      val iso2: TIso[Unit, (Int, Long, String)]   = iso1 >>> flatten
      verify(iso2, (), (1, 2L, "s"))

      val iso3: TIso[(Int, Long, String), Unit]   = ignore((1, 2L, "s"))
      val iso4: TIso[(Int, (Long, String)), Unit] = iso3 <<< flatten
      verify(iso4, (1, (2L, "s")), ())
    }

    "list" in {
      verify(list[Int], Left(()), Nil)
      verify(list[Int], Right((1, Nil)), List(1))
      verify(list[Int], Right((2, List(1))), List(2, 1))
    }

    "iterate" in {
      val iterator: TIso[Int, Int] = liftF(
        a => Some(a + 1).filter(_ <= 5),
        b => Some(b - 1).filter(_ >= 0)
      )
      iterate(iterator).to(2) must_=== Some(5)
      iterate(iterator).to(7) must_=== Some(7)
      iterate(iterator).from(2) must_=== Some(0)
      iterate(iterator).from(-7) must_=== Some(-7)
    }
  }
}
