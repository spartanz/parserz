package scalaz.parsers

import scalaz.parsers.tc.Alternative

import scala.collection.mutable

object cfg {

  sealed trait CFG
  case class Input(tag: String)                  extends CFG
  case class Mapped(tag: String, node: CFG)      extends CFG
  case class Seq(tag: String, node: List[CFG])   extends CFG
  case class Alt(tag: String, node: List[CFG])   extends CFG
  case class Many(tag: String, node: CFG)        extends CFG
  case class Many1(tag: String, node: CFG)       extends CFG
  case class Delay(tag: String, node: () => CFG) extends CFG

  object CFG {

    def tag: CFG => String = {
      case Input(tag)     => tag
      case Mapped(tag, _) => tag
      case Seq(tag, _)    => tag
      case Alt(tag, _)    => tag
      case Many(tag, _)   => tag
      case Many1(tag, _)  => tag
      case Delay(tag, _)  => tag
    }

    def tagged(tag: String): CFG => CFG = {
      case d @ Input(_)     => d.copy(tag = tag)
      case d @ Mapped(_, _) => d.copy(tag = tag)
      case d @ Seq(_, _)    => d.copy(tag = tag)
      case d @ Alt(_, _)    => d.copy(tag = tag)
      case d @ Many(_, _)   => d.copy(tag = tag)
      case d @ Many1(_, _)  => d.copy(tag = tag)
      case d @ Delay(_, _)  => d.copy(tag = tag)
    }
  }

  case class CFGP[A](cfg: CFG) {

    def show: List[String] =
      CFGP.show(Nil)(List(cfg)).collect { case (n, v) if n.nonEmpty => s"<$n>$v" }.distinct
  }

  object CFGP {

    def parserOps[F[_], G[_], E](P: Parsing[F, G, E]): P.ParserOps[CFGP] =
      new P.ParserOps[CFGP] {
        override def zip[A, B](p1: CFGP[A], p2: CFGP[B]): CFGP[A /\ B] =
          CFGP(Seq("", List(p1.cfg, p2.cfg)))
        override def alt[A, B](p1: CFGP[A], p2: CFGP[B])(
          implicit AF: Alternative[F]
        ): CFGP[A \/ B] =
          CFGP(Alt("", List(p1.cfg, p2.cfg)))
        override def map[A, B](p: CFGP[A])(equiv: P.Equiv[A, B]): CFGP[B] =
          CFGP(Mapped("", p.cfg))
        override def list[A](p: CFGP[A])(implicit AF: Alternative[F]): CFGP[List[A]] =
          CFGP(Many("", p.cfg))
        override def nel[A](e: E)(p: CFGP[A])(implicit AF: Alternative[F]): CFGP[List[A]] =
          CFGP(Many1("", p.cfg))
        override def tagged[A](t: String)(p: CFGP[A]): CFGP[A] =
          p.copy(cfg = CFG.tagged(t)(p.cfg))
      }

    def show(z: List[String -> String])(cfg: List[CFG]): List[String -> String] = {
      val visited: mutable.Set[CFG] = mutable.Set.empty

      def show1(z: List[String -> String])(cfg: List[CFG]): List[String -> String] =
        cfg.foldLeft(z)(
          (acc, cfg) =>
            if (visited.contains(cfg)) acc
            else {
              visited += cfg
              (cfg match {
                case Input(_) =>
                  Nil
                case Mapped(tag, d) =>
                  show1(List(tag -> (" ::= " + show(d))))(List(d))
                case Seq(tag, ds) =>
                  show1(List(tag -> (" ::= " + ds.map(show).mkString(" "))))(ds)
                case Alt(tag, ds) =>
                  show1(List(tag -> (" ::= " + ds.map(show).mkString("(", " | ", ")"))))(ds)
                case Many(tag, d) =>
                  show1(List(tag -> (" ::= List(" + show(d) + ")")))(List(d))
                case Many1(tag, d) =>
                  show1(List(tag -> (" ::= NEL(" + show(d) + ")")))(List(d))
                case Delay(tag, d) => show1(List(tag -> (" ::= " + show(d()))))(List(d()))
              }) ::: acc
            }
        )

      show1(z)(cfg)
    }

    private def show(node: CFG): String = {
      val tag = CFG.tag(node)
      if (tag.nonEmpty) s"<$tag>"
      else
        node match {
          case Input(_)     => ""
          case Mapped(_, d) => show(d)
          case Seq(_, ds)   => ds.map(show).mkString(" ")
          case Alt(_, ds)   => ds.map(show).mkString("(", " | ", ")")
          case Many(_, d)   => "List(" + show(d) + ")"
          case Many1(_, d)  => "NEL(" + show(d) + ")"
          case Delay(_, d)  => show(d())
        }
    }
  }
}
