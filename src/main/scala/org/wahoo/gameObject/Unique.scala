package org.wahoo
package gameObject

/** Marks this object as the only object of type A in any given state.
  * Then this can be accessed by GameObject#the[A] and GameObject#withThe[A].
  * The type parameter A must be declared to be Unique of itself.
  */
trait Unique
