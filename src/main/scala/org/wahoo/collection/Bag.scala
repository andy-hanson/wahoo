package org.wahoo
package collection

//@todo: use TypeTag
import scala.collection.mutable, scala.reflect.ClassTag

object Bag {
	def empty[A >: Null : ClassTag](sizeHint:Int): Bag[A] = ArrayBag.empty[A](sizeHint)
	def empty[A >: Null: ClassTag]: Bag[A] = ArrayBag.empty[A]
	def single[A >: Null : ClassTag](a:A): Bag[A] = ArrayBag.single(a)
	def apply[A >: Null: ClassTag](xs:A*): Bag[A] = ArrayBag(xs:_*)


}

abstract class Bag[A] extends mutable.Traversable[A] {
	def +=(value:A)
	def -=(value:A)
	def mutateFilter(f:A => Boolean)

	def ++=(values:Traversable[A]) { values.foreach { v => this += v } }

	def contains(value:A): Boolean

	//@todo: choose between these 2 algorithms depending on my size.
	//def --=(values:Traversable[A]) { values.foreach { v => this -= v } }
	def --=(values:Traversable[A]) {
		val set = mutable.HashSet(values.toSeq:_*)
		require(set.forall { contains(_) })
		mutateFilter { !set.contains(_) }
	}

	def clear()
	def clear(initSize:Int) { clear() }

	/** Bags compare equal regardless of order. */
	override def equals(a:Any) = a match {
		case s:Traversable[_] if s.size == size =>
			toSet == s.toSet
		case _ => false
	}

		/*def toSet = {
		val s = mutable.Set.empty[A]
		foreach { s += _ }
		s
	}*/

	//def contains(a:A) = !forall(_ != a)
}
