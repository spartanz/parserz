package scalaz.parsers.tokenizer

import scalaz.zio.IO

object types {
  type t[S, F[_, _], E, G[_], T] = S => F[E, G[T]]
  // F: ApplicativeError, G: MonoidalFunctor, Foldable

  type Chunk  = Array[Byte]
  type Header = String // Obviously we can't define here

  type stringTok[E] = t[String, IO, E, List, String]
  type headerTok[E] = t[Chunk, IO, E, Set, Header]
}
