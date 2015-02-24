package org.wahoo
package collection

import scala.reflect.ClassTag

object SortedBag {
	def empty[A >: Null](implicit tag:ClassTag[A], ordering:Ordering[A]): SortedBag[A] =
		SortedArrayBag.empty[A](tag, ordering)
}

trait SortedBag[A] extends Bag[A] {
	def min: A
	def max: A
	def iterator: BufferedIterator[A]
}
