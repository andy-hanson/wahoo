package org.wahoo
package collection

import math.implicits._

import scala.compat.Platform.arraycopy,
			 scala.reflect.ClassTag

abstract class AbstractResizeableArray[A >: Null : ClassTag] private[collection](initSize:Int)
	extends Bag[A] {
	//array size can never go below 2 !
	require(initSize > 1)

	var array = new Array[A](initSize.nextPowerOf2)

	var _size = 0
	override def size = _size

	def clear() { clear(2) }
	override def clear(initSize:Int) {
		require(initSize.isPowerOf2 && initSize > 1)
		_size = 0
		array = new Array[A](initSize)
	}

	def foreach[U](f:A => U) {
		var idx = 0
		while (idx != _size) {
			f(array(idx))
			idx += 1
		}
	}

	def append(a:A) {
		val newSize = _size + 1
		ensureCapacity(newSize)
		array(_size) = a
		_size = newSize
	}

	def appendMultiple(as:Traversable[A]) {
		var rest = as.toArray
		val newSize = _size + rest.size
		ensureCapacity(newSize)
		arraycopy(rest, 0, array, _size, rest.size)
		_size = newSize
	}

	//@todo: faster; may end up doing 2 arraycopies here

	/** Put the object before the element currently at the given index.
		* So now this(idx) == a.
		*/
	def insertAt(idx:Int, a:A) {
		require(idx >= 0 && idx <= _size)
		var newSize = _size + 1
		ensureCapacity(newSize)
		arraycopy(array, idx, array, idx + 1, _size - idx)
		array(idx) = a
		_size = newSize
	}

	//@todo: shrink array
	def removeAt(idx:Int) {
		require(idx >= 0 && idx < _size)
		arraycopy(array, idx + 1, array, idx, _size - idx - 1)
		_size -= 1
		array(_size) = null
	}

	//@todo: shrink array
	def removeLast() = {
		_size -= 1
		val removed = array(_size)
		array(_size) = null
		removed
	}

	override def last = array(_size - 1)

	def indexOf(a:A): Int = {
		var idx = _size
		while (idx != 0) {
			idx -= 1
			if (array(idx) == a) return idx
		}
		throw new NoSuchElementException("$a not in $this")
	}

	def contains(a:A) = {
		try   { indexOf(a); true }
		catch { case e:NoSuchElementException => false }
	}

		/** Removes all elements not matching the given predicate.
		* Fills in the rest with nulls so removed objects can be garbage collected.
		*/
	def mutateFilter(f:A => Boolean) {
		var  readIdx = 0
		var writeIdx = 0
		while (readIdx != _size) {
			val elem = array(readIdx)
			if (f(elem)) {
				array(writeIdx) = elem
				writeIdx += 1
			}
			readIdx += 1
		}

		//Fill rest with nulls
		var nullIdx = writeIdx
		assert(writeIdx <= _size)
		while (nullIdx != _size) {
			array(nullIdx) = null
			nullIdx += 1
		}

		_size = writeIdx
	}

	private def ensureCapacity(amount:Int) {
		if ((amount & array.size) != 0 && amount != array.size) {
			assert(amount > array.size)
			growToCapacity(amount.nextPowerOf2)
		}
		assert(array.size >= amount)
	}

	private def growToCapacity(newCapacity:Int) {
		require(newCapacity.isPowerOf2)
		require(newCapacity > array.size)
		val old = array
		array = new Array[A](newCapacity)
		arraycopy(old, 0, array, 0, _size)
	}

	def iterator() = new ResizeableArrayIterator()

	class ResizeableArrayIterator extends BufferedIterator[A] {
		var idx = 0
		def hasNext = idx != _size
		def head = array(idx)
		def next = {
			val nex = array(idx)
			idx += 1
			nex
		}
	}
}
