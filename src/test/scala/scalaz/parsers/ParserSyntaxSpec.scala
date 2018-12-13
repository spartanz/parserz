package scalaz.parsers

import org.specs2.mutable.Specification
import scalaz.tc.{ Applicative, ApplicativeClass, instanceOf }

class ParserSyntaxSpec extends Specification {

  type Parser[A] = String => (String, List[A])

  object Parser extends ParserSyntax[Parser] with ParserIsoSyntax[Parser, Option, Option] {
    override val iso: IsoClass[Option, Option] =
      new IsoClass[Option, Option] {}
    override def delay[A](pa: => Parser[A]): Parser[A] =
      pa(_)
    override def isoMap[A, B](pa: Parser[A])(iso: Iso[Option, Option, A, B]): Parser[B] =
      s => pa(s) match { case (s1, aa) => s1 -> aa.map(iso.to).collect { case Some(b) => b } }
  }

  object PIso extends IsoClass[Option, Option]

  object Instances {
    import scalaz.Scalaz.monadApplicative

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
          ap(fa)(pure(f))
      }
    )

    implicit val alternativeParser: Alternative[Parser] = instanceOf(
      new AlternativeClass[Parser] {
        override def or[A](f1: Parser[A], f2: => Parser[A]): Parser[A] = { s =>
          val (s1, r1) = f1(s)
          val (s2, r2) = f2(s)
          if (s1.length <= s2.length) s1 -> r1 else s2 -> r2
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

  private val parseChar: Parser[Char] =
    s => s.headOption.fold("" -> List.empty[Char])(c => s.drop(1) -> List(c))

  private val describe: Iso[Option, Option, Char, String] =
    iso.lift[Char, String]("char(" + _ + ")", _.drop(5).headOption.getOrElse(0))

  "isoMap" should {
    "map over parsed value with an iso" in {
      isoMap(parseChar)(describe)("a") must_=== (("", List("char(a)")))
      (parseChar ∘ describe)("a") must_=== (("", List("char(a)")))
      (parseChar ∘ describe)("ab") must_=== (("b", List("char(a)")))
    }
  }

  "many" should {
    "yield a list of things from one thing" in {
      parseChar.many.apply("ab") must_=== (("", List(List('a', 'b'))))
    }
  }
}
