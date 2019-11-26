package org.spartanz.parserz

trait ExprModule {

  sealed trait Expr[A]
  case class Equals[A](a: A) extends Expr[A]
  case class InSet[A](as: Set[A]) extends Expr[A]

  object Expr extends ExprSyntax {
    def === [A](a: A): Expr[A] = Equals(a)
    def in[A](a1: A, an: A*): Expr[A] = InSet(Set(an: _*) + a1)
  }

  // todo: not needed?
  trait ExprSyntax {
    implicit final class ToExprOps1[A](self: A) {
//      def === (a: A): Expr[A] = Equals(self, b)
//      def foo (b: A): Expr[A] = Equals(self, b)
    }
  }

  def exprFilter[A](expr: Expr[A]): A => Boolean =
    expr match {
      case Equals(a) => _ == a
      case InSet(as) => as.contains
    }

  def exprBNF[A](expr: Expr[A]): String =
    expr match {
      case Equals(a) => "\"" + a.toString + "\""
      case InSet(as) => as.map(_.toString).toList.sorted.mkString("( \"", "\" | \"" , "\" )")
    }
}
