package org.spartanz.parserz

trait ExprModule {

  sealed trait Expr[A]
  case class Equals[A](a: A) extends Expr[A]

  object Expr extends ExprSyntax {
    def === [A](a: A): Expr[A] = Equals(a)
  }

  trait ExprSyntax {
    implicit final class ToExprOps1[A](self: A) {
//      def === (a: A): Expr[A] = Equals(self, b)
//      def foo (b: A): Expr[A] = Equals(self, b)
    }
  }

  def exprFilter[A](expr: Expr[A]): A => Boolean =
    expr match {
      case Equals(a) => _ == a
    }

  def exprBNF[A](expr: Expr[A]): String =
    expr match {
      case Equals(a) => "\"" + a.toString + "\""
    }
}
