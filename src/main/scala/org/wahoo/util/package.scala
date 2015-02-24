package org.wahoo

import
	scala.io.Source,
	scala.collection.mutable,
	scala.math.Ordering.Implicits._

/** Helper functions. */
package object util {
	def packageToDirectoryName(klass:Class[_]) =
		klass.getPackage.getName.replace('.', '/')

	/** Every class from klass up to (but not including) upTo. */
	def allSuperclassesUpTo[A <: B, B](klass:Class[A], upTo:Class[B]): Iterable[Class[_ <: B]] =
	{
		var k: Class[_ <: B] = klass
		var out = mutable.ArrayBuffer[Class[_ <: B]]()
		while (k != upTo) {
			out += k
			k = k.getSuperclass.asInstanceOf[Class[_ <: B]]
		}
		out
	}

	def join(joiner:String, objs:Seq[Any]) =
		if (objs.isEmpty)
			""
		else objs.tail.foldLeft(objs.head.toString) { (a:String, b:Any) =>
			a + joiner + b.toString
		}

	def classToStringUpTo[B](klass:Class[_], upTo:Class[B]) =
	{
		require(upTo.isAssignableFrom(klass),
			s"$klass is not a subclass of $upTo")

		val klasses =
			(for (k <- allSuperclassesUpTo(klass.asInstanceOf[Class[_ <: B]], upTo))
				yield k.getSimpleName
			).toArray.reverse

		join("/", klasses)
	}

	def memoize[In, Out](f:In => Out): (In => Out) = new mutable.HashMap[In, Out]
	{
		override def apply(x:In): Out = getOrElseUpdate(x, f(x))
	}

	def memoize[In1, In2, Out](f: (In1, In2) => Out): ((In1, In2) => Out) =
	{
		val fmem = memoize[(In1, In2), Out] { xy => f(xy._1, xy._2) }
		(x, y) => fmem((x, y))
	}

	def memoizeWeak[In, Out](f:In => Out): (In => Out) =
		new mutable.WeakHashMap[In, Out] {
			override def apply(x:In): Out = getOrElseUpdate(x, f(x))
		}

	private def foreachMergedSortedNoCheck[A](
		iters:Traversable[BufferedIterator[A]])(
		f:A => Unit)(
		implicit ordering:Ordering[A])
	{
		val pq =
			mutable.PriorityQueue[BufferedIterator[A]]()(
				(ordering on { iter:BufferedIterator[A] => iter.head }).reverse
			)
		pq.sizeHint(iters.size)

		for (iter <- iters if (iter.hasNext))
			pq enqueue iter

		while (pq.nonEmpty) {
			val it = pq.dequeue()
			f(it.next())
			if (it.hasNext)
				pq.enqueue(it)
		}
	}

	def foreachMergedSorted[A >: Null](
		iters:Traversable[BufferedIterator[A]])(
		f:A => Unit)(
		implicit ordering:Ordering[A])
	{
		var last:A = null
		foreachMergedSortedNoCheck(iters) { a =>
			assert(last == null || last <= a,
				s"surprisingly, $last > $a")
			f(a)
			last = a
		}(ordering)
	}

	def assertSorted[A](
		iter: => BufferedIterator[A])(
		implicit ordering:Ordering[A])
	{
		if (iter.nonEmpty)
			iter.fold(iter.head) { (a, b) => assert(a <= b); b }
	}
}
