package scalaz.parsers

import org.specs2.mutable.Specification
import scalaz.parsers.tc.{ Alternative, Category }
import scalaz.std.either._

class DocumentationExampleSpec extends Specification {

  object Syntax {
    sealed trait Expression
    case class Constant(value: Int)                                    extends Expression
    case class Operation(e1: Expression, op: Operator, e2: Expression) extends Expression

    sealed trait Operator
    case object Add extends Operator
    case object Mul extends Operator
  }

  object env {
    import Category._

    val parsing: Parsing[Either[String, ?], Either[String, ?], String] = Parsing()
    val equiv: parsing.Equiv.type                                      = parsing.Equiv
    val syntax: parsing.syntax.type                                    = parsing.syntax

    type Equiv[A, B] = parsing.Equiv[A, B]
  }

  abstract class Grammar[P[_]: env.parsing.ParserOps] {
    import env.Equiv
    import env.equiv._
    import env.syntax._
    import Syntax._
    import TCInstances.naturalTransformationLoop

    val constantEq: Equiv[Int, Constant] =
      lift(Constant, _.value)

    val constantExpressionEq: Equiv[Constant, Expression] =
      liftPartial("Expected: Constant")(
        { case a               => a },
        { case n @ Constant(_) => n }
      )

    def operationExpressionEq(
      op: Operator
    ): Equiv[Expression /\ (Operator /\ Expression), Expression] =
      liftPartial("Expected: Operation")(
        { case (e1, (`op`, e2))        => Operation(e1, op, e2) },
        { case Operation(e1, `op`, e2) => e1 -> (op -> e2) }
      )

    def char: P[Char]

    val digit: P[Char] = "digit" @@ (char ∘ ensure("Expected: [0-9]")(_.isDigit))

    val plus: P[Operator] = "+" @@ (char ∘ liftPartial("Expected: '+'")(
      { case '+' => Add }, { case Add => '+' }
    ))

    val star: P[Operator] = "*" @@ (char ∘ liftPartial("Expected: '*'")(
      { case '*' => Mul }, { case Mul => '*' }
    ))

    // todo: how to get this error?
    val integer: P[Int] = "integer" @@ (digit.many1("Expected at least one digit") ∘ lift(
      chars => new String(chars.toArray).toInt,
      int => int.toString.toList
    ))

    val constant: P[Constant] = integer ∘ constantEq

    val case0: P[Expression] = "Constant" @@ (constant ∘ constantExpressionEq)

    // todo: how to get this error?
    val case1: P[Expression] = (case0 ~ (star ~ case0).many) ∘ foldl("hm...")(
      operationExpressionEq(Mul)
    )

    val case2: P[Expression] = (case1 ~ (plus ~ case1).many) ∘ foldl("hmm..")(
      operationExpressionEq(Add)
    )

    lazy val expression: P[Expression] = "Expression" @@ case2
  }

  object Parsers {
    import env.equiv
    import env.parsing.Codec
    import env.parsing.Codec._

    object Parser extends Grammar[Codec[List[Char], ?]] {
      override def char: Codec[List[Char], Char] = Codec(
        equiv.liftF(
          { case c :: cs => Right(cs -> c); case Nil => Left("Empty input") },
          { case (cs, c) => Right(cs :+ c) }
        )
      )
    }

    sealed trait Desc
    case class Input(name: String)                 extends Desc
    case class Mapped(name: String, desc: Desc)    extends Desc
    case class Seq(name: String, desc: List[Desc]) extends Desc
    case class Alt(name: String, desc: List[Desc]) extends Desc
    case class Many(name: String, desc: Desc)      extends Desc
    case class Many1(name: String, desc: Desc)     extends Desc

    object Desc {

      def name: Desc => String = {
        case Input(name)     => name
        case Mapped(name, _) => name
        case Seq(name, _)    => name
        case Alt(name, _)    => name
        case Many(name, _)   => name
        case Many1(name, _)  => name
      }

      def named(n: String): Desc => Desc = {
        case d @ Input(_)     => d.copy(name = n)
        case d @ Mapped(_, _) => d.copy(name = n)
        case d @ Seq(_, _)    => d.copy(name = n)
        case d @ Alt(_, _)    => d.copy(name = n)
        case d @ Many(_, _)   => d.copy(name = n)
        case d @ Many1(_, _)  => d.copy(name = n)
      }

      def show(desc: Desc): String = {
        val name = Desc.name(desc)
        if (name.nonEmpty) s"<$name>"
        else
          desc match {
            case Input(_)     => ""
            case Mapped(_, d) => show(d)
            case Seq(_, ds)   => ds.map(show).mkString(" ")
            case Alt(_, ds)   => ds.map(show).mkString("(", " | ", ")")
            case Many(_, d)   => "List(" + show(d) + ")"
            case Many1(_, d)  => "NEL(" + show(d) + ")"
          }
      }
    }

    import env.parsing.Equiv
    import env.parsing.ParserOps

    object Docs extends Grammar[Doc] {
      override def char: Doc[Char] = Doc(Input("char"))
    }

    case class Doc[A](desc: Desc) {

      def bnf: List[String] =
        Doc.bnf(Nil)(List(desc)).collect { case (n, v) if n.nonEmpty => s"<$n>$v" }.distinct
    }

    object Doc {

      def bnf(z: List[String -> String])(desc: List[Desc]): List[String -> String] =
        desc.foldLeft(z)(
          (acc, dd) =>
            (dd match {
              case Input(_)        => Nil
              case Mapped(name, d) => bnf(List(name -> (" ::= " + Desc.show(d))))(List(d))
              case Seq(name, ds) =>
                bnf(List(name -> (" ::= " + ds.map(Desc.show).mkString(" "))))(ds)
              case Alt(name, ds) =>
                bnf(List(name -> (" ::= " + ds.map(Desc.show).mkString("(", " | ", ")"))))(ds)
              case Many(name, d)  => bnf(List(name -> (" ::= List(" + Desc.show(d) + ")")))(List(d))
              case Many1(name, d) => bnf(List(name -> (" ::= NEL(" + Desc.show(d) + ")")))(List(d))
            }) ::: acc
        )

      implicit def parserOps[I]: ParserOps[Doc] = new ParserOps[Doc] {
        type F[A] = Either[String, A]
        override def zip[A, B](p1: Doc[A], p2: Doc[B]): Doc[A /\ B] =
          Doc(Seq("", List(p1.desc, p2.desc)))
        override def alt[A, B](p1: Doc[A], p2: Doc[B])(implicit AF: Alternative[F]): Doc[A \/ B] =
          Doc(Alt("", List(p1.desc, p2.desc)))
        override def map[A, B](p: Doc[A])(equiv: Equiv[A, B]): Doc[B] =
          Doc(Mapped("", p.desc))
        override def list[A](p: Doc[A])(implicit AF: Alternative[F]): Doc[List[A]] =
          Doc(Many("", p.desc))
        override def nel[A](e: String)(p: Doc[A])(implicit AF: Alternative[F]): Doc[List[A]] =
          Doc(Many1("", p.desc))
        override def tagged[A](t: String)(p: Doc[A]): Doc[A] =
          p.copy(desc = Desc.named(t)(p.desc))
      }
    }
  }

  "Docs" should {
    "be available for combinators" in {
      Parsers.Docs.integer.bnf.mkString("\n", "\n", "\n") must_===
        """
          |<digit> ::= <char>
          |<integer> ::= NEL(<digit>)
          |""".stripMargin
    }
    "be available for all expression" in {
      Parsers.Docs.expression.bnf.mkString("\n", "\n", "\n") must_===
        """
          |<digit> ::= <char>
          |<integer> ::= NEL(<digit>)
          |<Constant> ::= <integer>
          |<*> ::= <char>
          |<+> ::= <char>
          |<Expression> ::= <Constant> List(<*> <Constant>) List(<+> <Constant> List(<*> <Constant>))
          |""".stripMargin
    }
  }

  def parse(s: String): Either[String, (String, Syntax.Expression)] =
    Parsers.Parser.expression.parse(s.toCharArray.toList).map {
      case (cs, res) => cs.mkString -> res
    }

  def print(exp: Syntax.Expression): Either[String, String] =
    Parsers.Parser.expression.print(exp, Nil).map(_.mkString)

  "Parser" should {
    import Syntax._

    "not parse empty input" in {
      parse("") must_=== Left("Empty input")
    }
    "parse a digit into a literal" in {
      parse("5") must_=== Right("" -> Constant(5))
    }
    "parse several digits" in {
      parse("567") must_=== Right("" -> Constant(567))
    }

    "not parse a letter and indicate failure" in {
      parse("A") must_=== Left("Expected: [0-9]")
    }
    "not parse '+' by itself" in {
      parse("+") must_=== Left("Expected: [0-9]")
    }

    "parse operation with 2 numbers" in {
      parse("5+6") must_=== Right("" -> Operation(Constant(5), Add, Constant(6)))
      parse("5*6") must_=== Right("" -> Operation(Constant(5), Mul, Constant(6)))
    }
    "parse operation with 3 numbers" in {
      parse("5+6+7") must_=== Right(
        "" -> Operation(Operation(Constant(5), Add, Constant(6)), Add, Constant(7))
      )
      parse("5*6*7") must_=== Right(
        "" -> Operation(Operation(Constant(5), Mul, Constant(6)), Mul, Constant(7))
      )
    }

    "parse till it can" in {
      parse("1**2") must_=== Right("**2"   -> Constant(1))
      parse("1*2**3") must_=== Right("**3" -> Operation(Constant(1), Mul, Constant(2)))
    }

    "parse expressions" in {
      parse("12*34+56") must_=== Right(
        "" -> Operation(Operation(Constant(12), Mul, Constant(34)), Add, Constant(56))
      )
      parse("12+34*56") must_=== Right(
        "" -> Operation(Constant(12), Add, Operation(Constant(34), Mul, Constant(56)))
      )
      parse("1*2+3*4") must_=== Right(
        "" -> Operation(
          Operation(Constant(1), Mul, Constant(2)),
          Add,
          Operation(Constant(3), Mul, Constant(4))
        )
      )
    }
  }

  "Printer" should {
    import Syntax._

    "print a single-digit number" in {
      print(Constant(1)) must_=== Right("1")
    }
    "print a multi-digit number" in {
      print(Constant(23)) must_=== Right("23")
    }

    "print an operation" in {
      print(Operation(Constant(1), Add, Constant(2))) must_=== Right("1+2")
      print(Operation(Constant(1), Mul, Constant(2))) must_=== Right("1*2")
      print(Operation(Operation(Constant(1), Add, Constant(2)), Add, Constant(3))) must_=== Right(
        "1+2+3"
      )
      print(Operation(Constant(1), Add, Operation(Constant(2), Mul, Constant(3)))) must_=== Right(
        "1+2*3"
      )
      print(Operation(Operation(Constant(1), Mul, Constant(2)), Add, Constant(3))) must_=== Right(
        "1*2+3"
      )
      print(
        Operation(
          Operation(Operation(Constant(1), Add, Constant(2)), Add, Constant(3)),
          Add,
          Constant(4)
        )
      ) must_=== Right(
        "1+2+3+4"
      )
      print(
        Operation(
          Operation(Constant(1), Mul, Constant(2)),
          Add,
          Operation(Constant(3), Mul, Constant(4))
        )
      ) must_=== Right(
        "1*2+3*4"
      )
    }

    "not print an incorrectly composed expression" in {
      print(Operation(Constant(1), Add, Operation(Constant(2), Add, Constant(3)))) must_=== Left(
        "Expected: Constant"
      )
      print(
        Operation(
          Operation(Constant(1), Add, Constant(2)),
          Add,
          Operation(Constant(3), Add, Constant(4))
        )
      ) must_=== Left("Expected: Constant")
    }
  }
}
