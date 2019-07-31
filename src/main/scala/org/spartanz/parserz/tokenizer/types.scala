package org.spartanz.parserz.tokenizer

object types {
  type t[S, F[_, _], E, G[_], T] = S => F[E, G[T]]
  // F: ApplicativeError, G: MonoidalFunctor, Foldable

  type Chunk  = Array[Byte]
  type Header = String // Obviously we can't define here

  type stringTok[F[_, _], E] = t[String, F, E, List, String]
  type headerTok[F[_, _], E] = t[Chunk, F, E, Set, Header]
}
