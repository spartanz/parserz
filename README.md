Welcome to Parserz
===

#### zero dependency invertible parser combinators library for Scala


[![License](https://img.shields.io/badge/license-Apache%202.0-green)](https://opensource.org/licenses/Apache-2.0)
[![Build Status](https://travis-ci.org/spartanz/parserz.svg?branch=master)](https://travis-ci.org/spartanz/parserz)
[![CodeCov](https://codecov.io/gh/spartanz/parserz/branch/master/graph/badge.svg)](https://codecov.io/gh/spartanz/parserz)
[![Gitter](https://badges.gitter.im/spartanz/community.svg)](https://gitter.im/spartanz/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

[![Maven Central](https://img.shields.io/maven-central/v/org.spartanz/parserz_2.12.svg?label=maven%20central%202.12)](https://search.maven.org/search?q=g:%22org.spartanz%22%20AND%20a:%22parserz_2.12%22)
[![Maven Central](https://img.shields.io/maven-central/v/org.spartanz/parserz_2.13.svg?label=maven%20central%202.13)](https://search.maven.org/search?q=g:%22org.spartanz%22%20AND%20a:%22parserz_2.13%22)

---


## Summary

Parserz is a purely-functional library for creating parsers, pretty-printers, and grammar definitions
 from a single, type-safe specification of a grammar.

The main idea of the library is to facilitate conversions between types `A` and `B`
 in both directions via single coherent implementation, 
 as in contrast of having two separate implementations for `A => B` and `B => A`.
 
The theory behind this idea is outlined in the paper 
[Invertible Syntax Descriptions](https://www.informatik.uni-marburg.de/~rendel/unparse/rendel10invertible.pdf)
but the library does not exactly follow the implementation proposed in paper.



## Overview and highlights

The following are the abstractions provided by library
```scala
trait ParsersModule {
  type Input
  sealed abstract class Grammar[-SI, +SO, +E, A] {}
}
```

`Grammar` is a data type for modelling user-defined grammars and 
`ParsersModule` is a module that contains interpreters of `Grammar` into runnable functions, i.e. parsers and printers, etc.

Internally library follows the "free encoding" design where `Grammar` is defined as a generalized algebraic data type (GADT),
and all interpreters are implemented as traversals of `Grammar`.

Types:
 - `Input` - the **input** type; values of this type are consumed by parser and produced by printer
 - `A` - the **output** type; values of this type are produced by parser and consumed by printer
 - `E` - the **error** type; values of this type are produced by both parser and printer if they cannot proceed normally
 - `SI` - the **input state** type and
 - `SO` - the **output state** type; values of these types represent state that is threaded through the execution of parser and printer

Note: for the printer or other interpreters, meaning of `Input` and `A` may be completely reversed.
Unfortunately, Scala does not allow redefining type names based on one's interpretation, 
 so we have chosen the parser direction to be the titular one simply because library name is `parserz`.



## Getting the library

Simply add 
```
libraryDependencies += "org.spartanz" %% "parserz" % "0.2.0"
```
to the SBT build.



## Using the library

Instantiate the module
```scala
import org.spartanz.parserz.ParsersModule

object MyParser extends ParsersModule {
  override type Input = List[Char]
}
```
which also requires defining the input type.

Import combinators
```scala
import MyParser.Grammar._
import MyParser._
```

It is now possible to create grammars!

Let's create building blocks to parse the following program
```scala
"a(bx(),cy(z()))"
```
into description on how it can be invoked
```scala
case class Call(name: ::[Char], args: List[Call])
```

Full working example is [here](https://github.com/spartanz/parserz/blob/master/src/test/scala/org/spartanz/parserz/CallsExampleSpec.scala)


### Consuming input

`consume` combinator and its variants are used to take a portion of input
 and return remainder of the input and the consumed value. This is how it's defined:

```scala
final def consume[E, A](to: Input => E \/ (Input, A), from: ((Input, A)) => E \/ Input): Grammar[Any, Nothing, E, A]
```

It requires two functions to be provided both returning either a value or an error, for example

```scala
val char: G[Char] = consume({
  case c :: cs => Right(cs -> c)
  case Nil     => Left("eoi")
}, {
  case (cs, c) => Right(c :: cs)
})
```

The `G[A]` is simply a type alias for `Grammar[Any, Nothing, String, A]`, which means
 - state is not used (accepts any state and returns nothing)
 - error type is set to `String`
 - the value taken by this grammar is of type `List[Char]` (as defined above) and produced value is of type `A`


### Asserting

There are combinators that allow to assert the value (and return error if assertion failed),
 for example `filter` and its variants, which is defined as
 ```scala
final def filter[E1 >: E](e: E1)(f: A => Boolean): Grammar[SI, SO, E1, A]
```
It accepts a predicate function which is applied to the result of existing grammar.
For example to test that consumed character is something specific we can do

```scala
val alpha: G[Char]  = char.filter("expected: alphabetical")(_.isLetter)
val comma: G[Char]  = char.filter("expected: comma")(_ == ',')
```

Resulting grammars are new bigger building blocks.


### Sequencing, alterating and repeating

 - `~` or `zip` combines two grammars that produce `A` and `B` into a grammar that produces `Tuple2[A, B]`
 - `|` or `alt` combines two grammars that produce `A` and `B` into a grammar that produces `Either[A, B]`
 - `rep` repeats the grammar zero or more times while it can and 
 - `rep1` repeats the grammar one or more times while it can

In the program `"a(bx(),cy(z()))"`, `"bx(),cy(z())"` is the list of arguments of function `a`.
Here is how to parse them: 
it's a call to another function followed by zero or more calls to other functions prefixed by comma, or no calls at all:

```scala
val args: G[List[Call]] = ((call ~ (comma ~ call).rep) | succeed(Nil)).map({
  case Left((e1, en)) => e1 :: en.map(_._2)
  case Right(_)       => Nil
}, {
  case Nil            => Right(Nil)
  case e1 :: en       => Left((e1, en.map((',', _))))
})
```


### Recursive grammars

In the previous example there is a call to `call` grammar that produces a value of `Call`.
Call is also a recursive data structure, as it references itself:
```scala
case class Call(name: ::[Char], args: List[Call])
```

To allow recursion in grammar definitions, current version of library has the `delay` combinator. Here is how 
 grammar for `Call` is defined:
 
 ```scala
lazy val call: G[Call] = delay {
  (alpha.rep1 ~ paren1 ~ args ~ paren2).map(
    { case (((name, _), exp), _) => Call(name, exp) },
    { case Call(name, exp)       => (((name, '('), exp), ')') }
  )
}
``` 

Notice the `lazy` modifier as well which allows this grammar to be forward-referenced.


### Interpreters

All grammars are now constructed and can be passed to interpreters.

#### Parsing

The parser is simply a function constructed by the library from the given grammar:
```scala
val parser: (Unit, List[Char]) => (Unit, String \/ (List[Char], Call)) = MyParser.parser(call)
```

Passing program `"a(bx(),cy(z()))"` into this function (along with state of `Unit`) yields
```scala
Call(::('a', Nil), List(
  Call(::('b', List('x')), Nil), 
  Call(::('c', List('y')), List(
    Call(::('z', Nil), Nil)
  ))
))
```
which is indeed the structure representing function calls in the program.


#### Printing

The printer is also a function constructed by the library from the given grammar:
```scala
val printer: (Unit, (List[Char], Call)) => (Unit, String \/ List[Char]) = MyParser.printer(call)
```

Printing the value just received from the parser yields the program text back:
```scala
printer((), (Nil, value))._2.map(_.reverse.mkString)
```

Here `Nil` is initial output, which is, of course, empty,
 and there is a `reverse` done on the output because of the `consume` implementation above which prepends to list.


#### Grammar definition

There is an interpreter that produces a description of the grammar in the Backus–Naur form (BNF).

```scala
val description: List[String] = MyParser.bnf(call)
```

For it to work, grammars can be described with the `tag` combinator (or its symbolic equivalent `@@`):

```scala
val char: G[Char] = "char" @@ consume(...)
val alpha: G[Char]  = char.filter("expected: alphabetical")(_.isLetter).tag("alpha")
```

It interprets directly into a value, which is a printout of all tagged grammars. 
This is how it looks like for the example above

```
<alpha>       ::= <char>
<open paren>  ::= <char>
<comma>       ::= <char>
<args>        ::= (<call> List(<comma> <call>) | )
<close paren> ::= <char>
<call>        ::= NEL(<alpha>) <open paren> <args> <close paren>
```

