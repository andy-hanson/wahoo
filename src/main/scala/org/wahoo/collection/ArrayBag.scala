package org.wahoo
package collection

import math.implicits._

import scala.collection.mutable,
			 scala.reflect.ClassTag

object ArrayBag {
	def empty[A >: Null : ClassTag](initSize:Int) = new ArrayBag[A](initSize)
	def empty[A >: Null : ClassTag] = new ArrayBag[A](2)
	def single[A >: Null : ClassTag](x:A) = {
		val a = empty[A]
		a += x
		a
	}
	def apply[A >: Null : ClassTag](xs:A*) = {
		throw new RuntimeException("don't call me!")
		val a = new ArrayBag[A](xs.size.nextPowerOf2)
		a ++= xs
		a
	}
}


class ArrayBag[A >: Null : ClassTag] private(initSize:Int)
																		 extends AbstractResizeableArray[A](initSize) {
	def +=(a:A) { append(a) }

	/** Find, then remove an element from the bag.
		* Writes over that location with null so removed object can be garbage collected.
		*/
	def -=(a:A) { removeAt(indexOf(a)) }

	override def ++=(objs:Traversable[A]) { appendMultiple(objs) }
}
