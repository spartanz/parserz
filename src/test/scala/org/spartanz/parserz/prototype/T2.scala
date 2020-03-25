package org.spartanz.parserz.prototype

import org.spartanz.parserz.ParsersModule

object T2 {

  object Module extends ParsersModule {
    override type Input = List[Char]
  }

  import Module.Grammar._
  import Module._
  import Tools._

  type S    = String
  type E    = String
  type G[A] = Grammar[S, S, E, A]

  val one: G[Int]     = "one" @@ consumePure(i => i.tail -> i.head.toString.toInt, { case (i, a) => i ::: a.toString.toList })
  val two: G[Boolean] = "two" @@ consumePure(i => i.tail -> (i.head == 'T'), { case (i, a) => i ::: (if (a) List('T') else List('F')) })
  val thr: G[String]  = "thr" @@ succeed("blah")

  case class Thing(idx: Int, exists: Boolean, name: String)

  implicit val thingEquiv: Equiv[((Int, Boolean), String), Thing] =
    Equiv.caseClass3((a, b, c) => Thing.apply(a, b, c), t => Thing.unapply(t))

  val g1: G[((Int, Boolean), String)] = one ~ two ~ thr

  val g2: G[Thing] = toZDirect(g1)
  val g3: G[Thing] = toZ(g1)


  object Tools {
    import Hacks._

    sealed trait Equiv[A, Repr]
    case class Eq3[Z, A, B, C](f: (A, B, C) => Z, g: Z => Option[(A, B, C)]) extends Equiv[((A, B), C), Z]

    object Equiv {
//      def caseClass1[Z, A](f: A => Z, g: Z => Option[A]): Equiv[A, Z] = ???
//      def caseClass2[Z, A, B](f: (A, B) => Z, g: Z => Option[(A, B)]): Equiv[(A, B), Z] = ???
      def caseClass3[Z, A, B, C](f: (A, B, C) => Z, g: Z => Option[(A, B, C)]): Equiv[((A, B), C), Z] = Eq3(f, g)
    }

    // using Equiv with prior optimization of Zip operations
    def toZ[A, AA, Z](g: G[A])(implicit equiv: Equiv[AA, Z], ev: AA <:< A): G[Z] = {
      val g1: Option[GADT.ZipUnsafe[S, S, E]] = equiv match {
        case Eq3(_, _) => zippy(3)(g).map(l => GADT.ZipUnsafe(l.toArray))
      }
//      g1.fold {
//        toZDirect(g)(equiv, ev)
//        ???
//      } { g2 =>
        equiv match {
          case eq: Eq3[Z, ta, tb, tc] =>
            GADT.Map[S, S, E, Array[Any], Z](
              g1.get,
              arr => {
                val Array(a: ta, b: tb, c: tc) = arr
                Right(eq.f(a, b, c))
              },
              z => {
                eq.g(z).map { case (a, b, c) => Array(a, b, c) }.toRight("some error")
              }
            )
        }
//      }
    }

    // using Equiv without any optimization of execution plan
    def toZDirect[A, AA, Z](g: G[A])(implicit equiv: Equiv[AA, Z], ev: AA <:< A): G[Z] =
      equiv match {
        case eq: Eq3[Z, ta, tb, tc] => GADT.Map[S, S, E, A, Z](g,
          { case ((a: ta, b: tb), c: tc) =>
            Right(eq.f(a, b, c))
          },
          z => {
            eq.g(z).map { case (a, b, c) => val a0: A = ((a, b), c); a0 }.toRight("some error")
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

  def main(args: Array[String]): Unit = {
    println(parser(g1)("", List('1', 'T')))
    println()
    println(parser(g2)("", List('1', 'T')))
    println()
    println(parser(g3)("", List('1', 'T')))
    println()

    println()
    println(printer(g1)("", (Nil, ((2, true), "printed"))))
    println()
    println(printer(g2)("", (Nil, Thing(2, true, "printed"))))
    println()
    println(printer(g3)("", (Nil, Thing(2, true, "printed"))))
    println()

    println()
    println(bnf("g1" @@ g1).mkString("\n"))
    println()
    println(bnf("g2" @@ g2).mkString("\n"))
    println()
    println(bnf("g3" @@ g3).mkString("\n"))
    println()
  }
}
