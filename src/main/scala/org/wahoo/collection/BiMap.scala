package org.wahoo
package collection

object BiMap {
	def apply[A, B](pairs:(A, B)*) = {
		val aToB = Map(pairs:_*)
		val bToA = Map(pairs.map(_.swap):_*)
		new SimpleBiMap(aToB, bToA)
	}
}

abstract class BiMap[A, B] {
	def get(a:A): Option[B]
	def get(b:B)(implicit d: DummyImplicit): Option[A]

	def apply(a:A): B
	def apply(b:B)(implicit d: DummyImplicit): A

	def keys  : Traversable[A]
	def values: Traversable[B]
}

class SimpleBiMap[A, B] private[collection](aToB:Map[A, B], bToA:Map[B, A]) extends BiMap[A, B] {
	def get(a:A) = aToB.get(a)
	def get(b:B)(implicit d: DummyImplicit) = bToA.get(b)

	def apply(a:A) = aToB(a)
	def apply(b:B)(implicit d: DummyImplicit) = bToA(b)

	def keys   = aToB.keys
	def values = bToA.keys
}
