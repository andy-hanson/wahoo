package org.wahoo
package collection

import math.implicits._

import scala.reflect.ClassTag,
			 scala.math.Ordering.Implicits._

object SortedArrayBag {
	def empty[A >: Null](initSize:Int)(implicit tag:ClassTag[A], ordering:Ordering[A]) =
		new SortedArrayBag[A](initSize)(tag, ordering)
	def empty[A >: Null](implicit tag:ClassTag[A], ordering:Ordering[A]) =
		new SortedArrayBag[A](2)(tag, ordering)
}


//@todo: override indexOf
class SortedArrayBag[A >: Null] private[collection](initSize:Int)(
																implicit tag:ClassTag[A],
																ordering:Ordering[A])
																extends AbstractResizeableArray[A](initSize)
																with SortedBag[A] {
	def +=(a:A) { insertAt(insertIndex(a), a) }

	/* Place to insert this new element before. */
	private def insertIndex(a:A): Int = {
		var minIdx = 0; var maxIdx = _size

		while (minIdx != maxIdx) {
			val midIdx = minIdx.averageInt(maxIdx)
			if (a < array(midIdx)) maxIdx = midIdx
			else                   minIdx = midIdx + 1
		}
		minIdx
	}

	def -=(a:A) { removeAt(indexOf(a)) }

	def min = array(0)
	def max = array(_size - 1)
}
