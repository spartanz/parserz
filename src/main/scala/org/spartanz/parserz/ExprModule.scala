package org.spartanz.parserz

trait ExprModule {

  sealed trait Expr[A]
  case class Equals[A](a: A)               extends Expr[A]
  case class Not[A](a: A)                  extends Expr[A]
  case class InSet[A](as: Set[A])          extends Expr[A]
  case class Condition[A](f: A => Boolean) extends Expr[A]

  object Expr {
    def === [A](a: A): Expr[A]            = Equals(a)
    def =!= [A](a: A): Expr[A]            = Not(a)
    def in[A](a1: A, an: A*): Expr[A]     = InSet(Set(an: _*) + a1)
    def cond[A](f: A => Boolean): Expr[A] = Condition(f)
  }

  def exprFilter[A](expr: Expr[A]): A => Boolean =
    expr match {
      case Equals(a)    => _ == a
      case Not(a)       => _ != a
      case InSet(as)    => as.contains
      case Condition(f) => f
    }

  def exprBNF[A](expr: Expr[A]): String =
    expr match {
      case Equals(a)    => "\"" + a.toString + "\""
      case Not(a)       => "- \"" + a.toString + "\""
      case InSet(as)    => as.map(_.toString).toList.sorted.mkString("( \"", "\" | \"", "\" )")
      case Condition(_) => ""
    }
}
