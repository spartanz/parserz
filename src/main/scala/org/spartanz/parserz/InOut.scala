package org.spartanz.parserz

import scala.util.control.NoStackTrace

object InOut {

  sealed trait Error extends RuntimeException with NoStackTrace
  case object NoInput extends Error

  private def unsafeCompare[A](arr1: Array[A], i1: Int, arr2: Array[A], i2: Int, len: Int): Boolean = {
    var i = 0
    while (i < len && arr1(i1 + i) == arr2(i2 + i)) i += 1
    i == len
  }


  sealed trait Consumer[+E, I, +A]
  object Consumer {

    sealed trait Mutable[+E, I, +A] extends Consumer[E, I, A] {
      type State // the implementation of State is mutable
      def create(): State
      def copy(s: State): State
      def needsMoreInput(s: State): Boolean
      def feed(s: State, input: I, i: Int): Int
      def finish(s: State): A
    }

    sealed trait Simple[+E, I, +A] extends Consumer[E, I, A] {
      def create(): Int
      def needsMoreInput(s: Int): Boolean
      def feed(s: Int, input: I, i: Int): Simple.CountAndState
      def finish(s: Int): A
    }
    object Simple {
      final type CountAndState = Long
      final def create(count: Int, state: Int): CountAndState = ((0xFFFFFFFFL & count) << 32) | (0xFFFFFFFFL & state)
      final def extractCount(cas: CountAndState): Int = (cas >>> 32).toInt
      final def extractState(cas: CountAndState): Int = cas.toInt
    }
  }

  sealed trait Producer[+E, -A, O]
  object Producer {
    sealed trait Mutable[+E, -A, O] extends Producer[E, A, O] {
      type State
      def create(a: A): State
      def copy(s: State): State
      def needsMoreOutput(s: State): Boolean
      def expel(s: State, output: O, i: Int): Int
    }

    sealed trait Simple[+E, -A, O] extends Producer[E, A, O] {
      def create(a: A): Int
      def needsMoreOutput(s: Int): Boolean
      def expel(s: Int, output: O, i: Int): Long
    }
  }

  case class One[+E, I, A](consumer: Consumer.Simple[E, I, A], condition: A => Boolean)
  case class Many[+E, I, A](consumer: Consumer.Mutable[E, I, A])
  case class Exact[+E, I, A](consumer: Consumer.Simple[E, I, A])


  object Chars {
    import Consumer._

    private val single: Simple[Nothing, Array[Char], Char] =
      new Simple[Nothing, Array[Char], Char] {
        final val create: Int = -1
        final def needsMoreInput(s: Int): Boolean = s == -1

        final def feed(s: Int, input: Array[Char], i: Int): Simple.CountAndState =
          try { Simple.create(1, input(i).toInt) }
          catch { case _: ArrayIndexOutOfBoundsException => -1L }

        final def finish(s: Int): Char =
          if (s == -1) throw NoInput
          else s.toChar
      }

    private def multiple(p: Char => Boolean): Mutable[Nothing, Array[Char], Array[Char]] = {
      class St(var acc: Array[Char], var done: Boolean)

      new Mutable[Nothing, Array[Char], Array[Char]] {
        final type State = St
        final def create(): State = new St(Array.emptyCharArray, false)
        final def copy(s: State): State = new St(s.acc, s.done)
        final def needsMoreInput(s: State): Boolean = !s.done

        final def feed(s: State, input: Array[Char], i: Int): Int = {
          val last = input.indexWhere(!p(_), i)
          val i2 = if (last == -1) input.length else last
          val chunk = java.util.Arrays.copyOfRange(input, i, i2)

          s.acc ++= chunk
          s.done = last != -1 || input.length == 0
          chunk.length
        }

        final def finish(s: State): Array[Char] =
          s.acc
      }
    }

    val one: One[Nothing, Array[Char], Char] =
      One(single, null)

    def oneIf(expr: Expr[Char]): One[Nothing, Array[Char], Char] =
      One(single, Expr.exprFilter(expr))

    def manyWhile(expr: Expr[Char]): Many[Nothing, Array[Char], Array[Char]] =
      Many(multiple(Expr.exprFilter(expr)))

    def exact(t: String): Exact[Nothing, Array[Char], Array[Char]] =
      Exact(
        new Simple[Nothing, Array[Char], Array[Char]] {
          private val req = t.toCharArray
          private val len = t.length

          // bit0: means "no match detected" if set
          // all other bits are number of chars consumed so far
          final val create: Int = 0
          final def needsMoreInput(s: Int): Boolean = (s >>> 31) == 0 && s < len

          final def feed(s: Int, input: Array[Char], i: Int): Long = {
            try { Simple.create(len, if (unsafeCompare(req, 0, input, i, len)) len else 0x80000000 | len) }
            catch { case _: ArrayIndexOutOfBoundsException =>
              // todo: consume available chars and update state, e.g.
              val consumed = 0
              val matching = true
              Simple.create(0, if (matching) consumed else 0x80000000 | consumed)
            }
          }

          final def finish(s: Int): Array[Char] = {
            if ((s >>> 31) == 1) null
            else if (s < len) throw NoInput
            else req
          }
        }
      )
  }
}
