package scalaz.parsers

import org.specs2.mutable.Specification
import scalaz.tc._

class ParserSyntaxSpec extends Specification {

  private type Parser[A] = String => (String, List[A])

  private object Parser extends ParserSyntax[Parser] with ParserIsoSyntax[Parser, Option, Option] {
    override val iso: IsoClass[Option, Option] =
      new IsoClass[Option, Option] {}
    override def delay[A](pa: => Parser[A]): Parser[A] =
      pa(_)
    override def isoMap[A, B](pa: Parser[A])(iso: Iso[Option, Option, A, B]): Parser[B] =
      s => pa(s) match { case (s1, aa) => s1 -> aa.map(iso.to).collect { case Some(b) => b } }
  }

  private object Instances {
    import scalaz.Scalaz.monadApplicative

    implicit val functorParser: Functor[Parser] = instanceOf(
      new FunctorClass[Parser] {
        override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
          s => fa(s) match { case (s1, aa) => s1 -> aa.map(f) }
      }
    )

    implicit val applicativeParser: Applicative[Parser] = instanceOf(
      new ApplicativeClass[Parser] {
        override def pure[A](a: A): Parser[A] =
          s => s -> List(a)
        override def ap[A, B](fa: Parser[A])(fab: Parser[A => B]): Parser[B] = s => {
          val (s1, aa) = fa(s)
          val (s2, ff) = if (aa.nonEmpty) fab(s1) else s1 -> Nil
          s2 -> ff.flatMap(aa.map(_))
        }
        override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
          functorParser.map(fa)(f)
      }
    )

    implicit val alternativeParser: Alternative[Parser] = instanceOf(
      new AlternativeClass[Parser] {
        override def or[A](f1: Parser[A], f2: => Parser[A]): Parser[A] = { s =>
          val (s1, r1) = f1(s)
          val (s2, r2) = f2(s)
          val diff     = s1.lengthCompare(s2.length)
          if (diff < 0) s1 -> r1 else if (diff > 0) s2 -> r2 else s1 -> (r1 ::: r2)
        }
      }
    )

    implicit val applicativeOption: Applicative[Option] =
      monadApplicative[Option](implicitly)
  }

  import Instances._
  import Parser._

  "delay" should {
    "make access to parser constructor stack safe" in {
      val p0: Parser[String]      = s => ("", List(s))
      lazy val p1: Parser[String] = p2
      lazy val p2: Parser[String] = p1
      // accessing `p1` via `delay` does not result in endless loop
      delay(p1) must_!== p0
    }
  }

  "lift" should {
    "construct a parser that yields given value" in {
      lift(1).apply("a") must_=== (("a", List(1)))
    }
  }

  // parser that consumes one char from the input
  private val char: Parser[Char] =
    s => s.headOption.map(c => s.drop(1) -> List(c)).getOrElse("" -> Nil)

  // iso that formats the char `x` as `char(x)`
  private val describe: Iso[Option, Option, Char, String] =
    iso.lift("char(" + _ + ")", _.drop(5).headOption.getOrElse(0))

  "isoMap" should {
    "map over parsed value with an iso" in {
      isoMap(char)(describe)("a") must_=== ("" -> List("char(a)"))
      (char ∘ describe)("a") must_=== (""      -> List("char(a)"))
      (char ∘ describe)("ab") must_=== ("b"    -> List("char(a)"))
    }
  }

  "many" should {
    "yield a list of results from input" in {
      char.many.apply("ab") must_=== (""              -> List(List('a', 'b')))
      (char ∘ describe).many.apply("ab") must_=== ("" -> List(List("char(a)", "char(b)")))
    }
  }

  "combinators" >> {
    "and (product)" in {
      (char /\ char).apply("a") must_=== (""  -> List())
      (char /\ char).apply("ab") must_=== ("" -> List('a' -> 'b'))
    }
    "or (coproduct)" in {
      (char \/ char).apply("a") must_=== (""       -> List(Left('a'), Right('a')))
      (char \/ char).apply("ab") must_=== ("b"     -> List(Left('a'), Right('a')))
      (char.many \/ char).apply("ab") must_=== ("" -> List(Left(List('a', 'b'))))
      (char \/ char.many).apply("ab") must_=== ("" -> List(Right(List('a', 'b'))))
    }
    "or (alternative)" in {
      (char || char).apply("") must_=== (""  -> Nil)
      (char || char).apply("a") must_=== ("" -> List('a', 'a'))

      def eq(c: Char): Char => Option[Char]            = Some(_).filter(_ == c)
      def is(c: Char): Iso[Option, Option, Char, Char] = iso.liftF(eq(c), eq(c))
      ((char ∘ is('a')) || (char ∘ is('b'))).apply("a") must_=== ("" -> List('a'))
    }
  }
}
