package scalaz

package object parsers {

  type /\[A, B] = (A, B)
  type \/[A, B] = Either[A, B]
}
