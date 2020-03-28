package org.spartanz.parserz.prototype

object T2 {

  object Module extends P2Module {
    override type Input = List[Char]
    override type S     = String
    override type E     = String
  }

  import Module.Grammar._
  import Module._
  import Tools._

  val one: G[Int]     = "one" @@ consumePure(i => i.tail -> i.head.toString.toInt, { case (i, a) => i ::: a.toString.toList })
  val two: G[Boolean] = "two" @@ consumePure(i => i.tail -> (i.head == 'T'), { case (i, a) => i ::: (if (a) List('T') else List('F')) })
  val thr: G[String]  = "thr" @@ succeed("blah")

  case class Thing(idx: Int, exists: Boolean, name: String)

  implicit val thingEquiv: Equiv[((Int, Boolean), String), Thing] =
    Equiv.caseClass3(Thing.apply, Thing.unapply(_).get)

  val g1: G[((Int, Boolean), String)] = one ~ two ~ thr

  val g2: G[Thing] = toZDirect(g1)
  val g3: G[Thing] = toZ(g1)


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
