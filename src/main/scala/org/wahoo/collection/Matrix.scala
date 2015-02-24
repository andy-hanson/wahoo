package org.wahoo
package collection

import math.implicits._

import scala.reflect.ClassTag

object Matrix {
	def apply[A : ClassTag](width:Int, height:Int)(filler: (Int, Int) => A) =
		new Matrix[A](width, height).fill(filler)
	def apply[A : ClassTag](width:Int, height:Int, filler: => A): Matrix[A] =
		apply(width, height)((xi, yi) => filler)
}

class Matrix[A : ClassTag] private(val width:Int, val height:Int) {
	val maxIndex = width * height
	private var arr = new Array[A](maxIndex)

	def fill(filler:(Int, Int) => A) = {
		eachIndices { (x, y) => this(x, y) = filler(x, y) }
		this
	}

	def eachIndices(f:(Int, Int) => Unit) {
		var y = 0
		while (y < height) {
			var x = 0
			while (x < width) {
				f(x, y)
				x += 1
			}
			y += 1
		}
	}

	def apply(x:Int, y:Int) = { require(isValidIndex(x, y)); arr(toIndex(x, y)) }
	def update(x:Int, y:Int, value:A) {
		require(isValidIndex(x, y)); arr(toIndex(x, y)) = value
	}

	def getLimited(x:Int, y:Int) = apply(limitX(x), limitY(y))

	//def get(x:Int, y:Int): Option[A] = {
	//  val idx = toIndex(x, y)
	//  if (isValidIndex(x, y)) Some(arr(idx)) else None
	//}

	def foreach(f:A => Unit) { arr.foreach(f) }

	def foreachWithIndices(f:(A, Int, Int) => Unit) {
		eachIndices { (x, y) => f(this(x, y), x, y) }
	}

	def toIndex(x:Int, y:Int) = x + y * width

	def isValidIndex(x:Int, y:Int) = x >= 0 && y >= 0 && toIndex(x, y) < maxIndex

	def limitX(x:Int) = x.limit(0, width  - 1)
	def limitY(y:Int) = y.limit(0, height - 1)

	def eachInSubMatrixLimited(x0:Int, y0:Int, x1:Int, y1:Int)(f:A => Unit) {
		eachInSubMatrix(limitX(x0), limitY(y0), limitX(x1), limitY(y1))(f)
	}

	/** Iterates through every point in range [x0, y0] and [x1, y1] inclusive. */
	def eachInSubMatrix(x0:Int, y0:Int, x1:Int, y1:Int)(f:A => Unit) {
		require(isValidIndex(x0, y0)); require(isValidIndex(x1, y1))

		var y = y0
		while (y <= y1) {
			var x = x0
			var idx = toIndex(x, y)
			while (x <= x1) {
				f(arr(idx))
				idx += 1; x += 1
			}
			y += 1
		}
	}

	def row(y:Int) = new Iterator[A] {
		var idx = y * width
		val end = idx + width
		def hasNext() = idx < end
		def next() = { val x = arr(idx); idx += 1; x }
	}

	def rows = new Iterator[Iterator[A]] {
		var y = 0
		def hasNext() = y < height
		def next() = { val x = row(y); y += 1; x }
	}

	override def toString = {
		val sb = new StringBuilder("Matrix(")
		for (row <- rows) {
			for (elt <- row) {
				sb ++= String.format("%8s", elt.toString)
			}
			sb += '\n'
		}
		sb.toString
	}
}
