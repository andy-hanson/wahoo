package org.wahoo
package collection

import math.implicits._

import scala.collection.mutable, scala.reflect.ClassTag

object FixedLengthQueue {
	def apply[A : ClassTag](size:Int)(filler:Int => A) = {
		val flq = new FixedLengthQueue(size)
		flq.fill(filler)
		flq
	}
	def apply[A : ClassTag](size:Int, filler: => A): FixedLengthQueue[A] =
		apply(size) { i => filler }
}

/** Implements a queue where the size is always the same.
	* Uses an array where enqueues and dequeues always happen at index.
	* This index cycles around the array.
	*/
class FixedLengthQueue[A : ClassTag] private(fixedSize:Int) extends mutable.Traversable[A] {
	require(fixedSize.positive)
	private val array = new Array[A](fixedSize)
	/** Next place to insert and delete. */
	private var index = 0

	override def size = array.size

	def fill(f: Int => A) {
		var i = 0
		while (i != size) {
			array(i) = f(i)
			i += 1
		}
	}

	/** Enqueues a new object and simultaneously returns a dequeued object. */
	def enqueueAndDequeue(toEnqueue:A) = {
		val dequeued = array(index)
		array(index) = toEnqueue

		index += 1
		if (index == size)
			index = 0
	}

	def foreach[U](f:A => U) {
		var i = index
		while (i > 0) { i -= 1; f(array(i)) }
		i = size
		while (i > index) { i -= 1; f(array(i)) }
	}
}
