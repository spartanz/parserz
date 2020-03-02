package org.spartanz.parserz

sealed abstract class SeparatedBy[+A, +S] {
  def values: List[A]
  def separators: List[S]
}

sealed abstract class SeparatedBy1[+A, +S] extends SeparatedBy[A, S] { self =>
  import SeparatedBy._

  final def prepend[A1 >: A, S1 >: S](a: A1, s: S1): SeparatedBy1[A1, S1] =
    Many(a, s, self)

  final def reverse: SeparatedBy1[A, S] = {
    @scala.annotation.tailrec
    def inner(values: List[A], separators: List[S], tail: SeparatedBy1[A, S]): SeparatedBy1[A, S] =
      values match {
        case Nil     => tail
        case a :: as => inner(as, separators.tail, SeparatedBy(a, separators.head, tail))
      }
    values.tail match {
      case Nil => SeparatedBy(values.head)
      case as  => inner(as, separators, SeparatedBy(values.head))
    }
  }
}

object SeparatedBy {

  def apply[A, S](): SeparatedBy[A, S] = Empty
  def apply[A, S](head: A): SeparatedBy1[A, S] = One(head)
  def apply[A, S](head: A, separator: S, tail: SeparatedBy1[A, S]): SeparatedBy1[A, S] = Many(head, separator, tail)

  case object Empty extends SeparatedBy[Nothing, Nothing] {
    val values: List[Nothing] = Nil
    val separators: List[Nothing] = Nil
  }

  case class One[+A](head: A) extends SeparatedBy1[A, Nothing] {
    val values: List[A] = List(head)
    val separators: List[Nothing] = Nil
  }

  case class Many[+A, +S](head: A, separator: S, tail: SeparatedBy1[A, S]) extends SeparatedBy1[A, S] {
    val values: List[A] = head :: tail.values
    val separators: List[S] = separator :: tail.separators
  }
}
