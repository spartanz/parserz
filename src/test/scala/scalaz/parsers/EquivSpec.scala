package scalaz.parsers

import org.specs2.mutable.Specification
import scalaz.parsers.tc.Category
import scalaz.std.option._

class EquivSpec extends Specification {

  import Category._
  import TCInstances._

  private val parsing: Parsing[Option, Option, Unit] = Parsing()
  private type Equiv[A, B] = parsing.Equiv[A, B]

  private def verify[A, B](equiv: Equiv[A, B], a: A, b: B) =
    (equiv.to(a).get must_=== b)
      .and(equiv.from(b).get must_=== a)
      .and(equiv.from(equiv.to(a).get).get must_=== a)

  "Constructors" >> {
    import parsing.Equiv._
    import parsing.Equiv.Product._

    "apply" in {
      verify(apply[Int, String](i => Some(i.toString), s => Some(s.toInt)), 2, "2")
    }

    "lift" in {
      verify(lift[Int, Int](_ + 1, _ - 1), 3, 4)
      verify(liftFG[Int, Int](a => Some(a + 1), b => Some(b - 1)), 3, 4)
    }

    "id" in {
      verify(id[Int], 2, 2)
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
    import parsing.syntax._
    import parsing.Equiv._

    val equiv1: Equiv[Int, Int] = lift(_ + 1, _ - 1)
    val equiv2: Equiv[Int, Int] = lift(_ + 2, _ - 2)

    "conjunction" >> {
      verify(equiv1 /\ equiv2, (0, 0), (1, 2))
    }

    "disjunction" >> {
      verify(equiv1 \/ equiv2, Left(0), Left(1))
      verify(equiv1 \/ equiv2, Right(0), Right(2))
    }

    "reverse" >> {
      verify(equiv1, 0, 1)
      verify(~equiv1, 1, 0)
    }

    "chain" >> {
      verify(equiv1 >>> equiv2, 0, 3)
      verify(equiv1 <<< equiv2, 0, 3)
      verify(~equiv1 >>> equiv2, 0, 1)
      verify(~equiv1 <<< equiv2, 0, 1)
      verify(~equiv1 >>> ~equiv2, 0, -3)
      verify(~equiv1 <<< ~equiv2, 0, -3)
    }
  }

  "Combinators" >> {
    import parsing.syntax._
    import parsing.Equiv._
    import parsing.Equiv.Product._

    "create" in {
      verify(create(5), (), 5)
    }

    "ignore" in {
      verify(ignore(5), 5, ())
    }

    "id" in {
      val equiv1: Equiv[Unit, Int] = create(5)
      val equiv2: Equiv[Int, Unit] = ignore(5)
      verify(equiv1 >>> id[Int], (), 5)
      verify(equiv2 <<< id[Int], 5, ())
    }

    "lift" in {
      val equiv1: Equiv[Unit, Int] = create(5)
      verify(equiv1 >>> lift[Int, Int](_ + 1, _ - 1), (), 6)
      verify(equiv1 >>> liftFG[Int, Int](a => Some(a + 1), b => Some(b - 1)), (), 6)
    }

    "unit" in {
      val equiv1: Equiv[Unit, Int] = create(5)
      verify(equiv1 >>> unitL, (), ((), 5))
      verify(equiv1 >>> unitR, (), (5, ()))
    }

    "commute" in {
      val equiv1: Equiv[Unit, (Int, String)] = create((1, "s"))
      val equiv2: Equiv[Unit, (String, Int)] = equiv1 >>> commute
      verify(equiv2, (), ("s", 1))

      val equiv3: Equiv[(Int, String), Unit] = ignore((1, "s"))
      val equiv4: Equiv[(String, Int), Unit] = equiv3 <<< commute
      verify(equiv4, ("s", 1), ())
    }

    "associate" in {
      val equiv1: Equiv[Unit, (Int, (Long, String))] = create((1, (2L, "s")))
      val equiv2: Equiv[Unit, ((Int, Long), String)] = equiv1 >>> associate
      verify(equiv2, (), ((1, 2L), "s"))

      val equiv3: Equiv[((Int, Long), String), Unit] = ignore(((1, 2L), "s"))
      val equiv4: Equiv[(Int, (Long, String)), Unit] = equiv3 <<< associate
      verify(equiv4, (1, (2L, "s")), ())
    }

    "flatten" in {
      val equiv1: Equiv[Unit, (Int, (Long, String))] = create((1, (2L, "s")))
      val equiv2: Equiv[Unit, (Int, Long, String)]   = equiv1 >>> flatten
      verify(equiv2, (), (1, 2L, "s"))

      val equiv3: Equiv[(Int, Long, String), Unit]   = ignore((1, 2L, "s"))
      val equiv4: Equiv[(Int, (Long, String)), Unit] = equiv3 <<< flatten
      verify(equiv4, (1, (2L, "s")), ())
    }

    "list" in {
      verify(list[Int], Right(()), Nil)
      verify(list[Int], Left((1, Nil)), List(1))
      verify(list[Int], Left((2, List(1))), List(2, 1))
    }

    "iterate" in {
      val iterator: Equiv[Int, Int] = liftFG(
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
