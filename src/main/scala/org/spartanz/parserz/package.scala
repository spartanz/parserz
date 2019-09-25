package org.spartanz

package object parserz {

  type ->[A, B]   = (A, B)
  type /\[A, B]   = (A, B)
  type \/[+A, +B] = Either[A, B]

  type =?>[-A, +B] = PartialFunction[A, B]
}
