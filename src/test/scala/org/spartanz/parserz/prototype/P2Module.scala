package org.spartanz.parserz.prototype

import org.spartanz.parserz.ParsersModule

trait P2Module extends ParsersModule {
  import Grammar._

  type S
  type E
  type G[A] = Grammar[S, S, E, A]

  object Tools {
    import Hacks._

    sealed trait Equiv[A, Repr]
    case class Equiv3[Z, A, B, C](f: (A, B, C) => Z, g: Z => (A, B, C)) extends Equiv[((A, B), C), Z]
    case class Equiv4[Z, A, B, C, D](f: (A, B, C, D) => Z, g: Z => (A, B, C, D)) extends Equiv[(((A, B), C), D), Z]

    object Equiv {
//      def caseClass1[Z, A](f: A => Z, g: Z => Option[A]): Equiv[A, Z] = ???
//      def caseClass2[Z, A, B](f: (A, B) => Z, g: Z => Option[(A, B)]): Equiv[(A, B), Z] = ???
      def caseClass3[Z, A, B, C](f: (A, B, C) => Z, g: Z => (A, B, C)): Equiv[((A, B), C), Z] = Equiv3(f, g)
      def caseClass4[Z, A, B, C, D](f: (A, B, C, D) => Z, g: Z => (A, B, C, D)): Equiv[(((A, B), C), D), Z] = Equiv4(f, g)
    }

    // using Equiv with prior optimization of Zip operations
    def toZ[A, AA, Z](g: G[A])(implicit equiv: Equiv[AA, Z], ev: AA <:< A): G[Z] = {
      val g1: Option[GADT.ZipUnsafe[S, S, E]] = equiv match {
        case Equiv3(_, _) => zippy(3)(g).map(l => GADT.ZipUnsafe(l.toArray))
        case Equiv4(_, _) => zippy(4)(g).map(l => GADT.ZipUnsafe(l.toArray))
      }
//      g1.fold {
//        toZDirect(g)(equiv, ev)
//        ???
//      } { g2 =>
        // place where we cast types back at runtime
        equiv match {
          case equiv: Equiv3[Z, ta, tb, tc] => GADT.Map[S, S, E, Array[Any], Z](g1.get,
            arr => {
              val Array(a: ta, b: tb, c: tc) = arr
              Right(equiv.f(a, b, c))
            },
            z => {
              val (a: ta, b: tb, c: tc) = equiv.g(z)
              Right(Array(a, b, c))
            }
          )
          case equiv: Equiv4[Z, ta, tb, tc, td] => GADT.Map[S, S, E, Array[Any], Z](g1.get,
            arr => {
              val Array(a: ta, b: tb, c: tc, d: td) = arr
              Right(equiv.f(a, b, c, d))
            },
            z => {
              val (a: ta, b: tb, c: tc, d: td) = equiv.g(z)
              Right(Array(a, b, c, d))
            }
          )
        }
//      }
    }

    // using Equiv without any optimization of execution plan
    def toZDirect[A, AA, Z](g: G[A])(implicit equiv: Equiv[AA, Z], ev: AA <:< A): G[Z] =
      equiv match {
        case equiv: Equiv3[Z, ta, tb, tc] => GADT.Map[S, S, E, A, Z](g,
          { case ((a: ta, b: tb), c: tc) => Right(equiv.f(a, b, c)) },
          z => {
            val (a: ta, b: tb, c: tc) = equiv.g(z)
            Right(((a, b), c))
          }
        )
        case equiv: Equiv4[Z, ta, tb, tc, td] => GADT.Map[S, S, E, A, Z](g,
          { case (((a: ta, b: tb), c: tc), d: td) => Right(equiv.f(a, b, c, d)) },
          z => {
            val (a: ta, b: tb, c: tc, d: td) = equiv.g(z)
            Right((((a, b), c), d))
          }
        )
      }
  }

  // place where we loose types
  object Hacks {
    def zippy[A](size: Int)(g: G[A]): Option[List[G[Any]]] = {

      @scala.annotation.tailrec
      def step[AA](i: Int)(acc: List[G[Any]])(g: G[AA]): List[G[Any]] =
        g match {
//          case GADT.ZipL(left, right, b) =>
//          case GADT.ZipR(left, right, a) =>
          case GADT.Zip(l, r) if i > 0 =>
            step(i - 1)(r.asInstanceOf[G[Any]] :: acc)(l)
          case g if i > 0 =>
            g.asInstanceOf[G[Any]] :: acc
          case _ =>
            acc
        }

      val list = step(size)(Nil)(g)
      if (list.length == size) Some(list) else None
    }
  }
}
