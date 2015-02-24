package org.wahoo
package collection

import math.implicits._,
			 math.Scalar,
			 shape.{Shape, Vec}

import scala.reflect.ClassTag

object SpaceMatrix
{
	def apply[A : ClassTag](size:Vec, elemSize:Scalar)(filler: Vec => A) =
	{
		val sm = new SpaceMatrix[A](size.x, size.y, elemSize)
		sm fill filler
		sm
	}

	def apply[A : ClassTag](size:Vec, elemSize:Scalar, filler: => A): SpaceMatrix[A] =
		apply(size, elemSize)(vec => filler)
}


//@todo: no (Scalar, Scalar) fns, just on Vecs.
class SpaceMatrix[A : ClassTag] private[collection](
	val width:Scalar,
	val height:Scalar,
	val elemSize:Scalar)
{
	//require(elemSize.isPowerOf2)
	require(width > elemSize, "Must be wider than element size")
	require((width / elemSize).isInteger, "Width must be multiple of element size")
	require(height > elemSize, "Must be taller than element size")
	require((height / elemSize).isInteger, "Height must be multiple of element size")

	def offsetX: Scalar = 0
	def offsetY: Scalar = 0

	private val matWidth  = (width  / elemSize).toInt
	private val matHeight = (height / elemSize).toInt
	private val maxIndex  = matWidth * matHeight
	private var array = new Array[A](maxIndex)

	//def fill(const:A) { mutateMap { (a, x, y) => const } }
	def fill(filler:Vec => A)
	{
		mutateMap { (a, vec) =>
			filler(vec)
		}
	}


	//@todo: why does this not work?
	//def indexOf(x:Scalar, y:Scalar) =
	//  ( ((x - offsetX) + (y - offsetY) * matWidth) / elemSize).toInt

	/** Closest node left of x and below y. */
	/*def indexOf(x:Scalar, y:Scalar) = {
		val xi = ( (x - offsetX) / elemSize ).toInt
		val yi = ( (y - offsetY) / elemSize ).toInt
		xi + yi * matWidth
	}*/
	def indexOf(x:Scalar, y:Scalar): Option[Int] = {
		val xi = ( (x - offsetX) / elemSize ).floor
		val yi = ( (y - offsetY) / elemSize ).floor

		if (xi.inRangeExcludingMax(0, matWidth) && yi.inRangeExcludingMax(0, matHeight))
			Some(yi * matWidth + xi)
		else
			None
	}


	def apply(v:Vec): A =
		apply(v.x, v.y)

	def apply(x:Scalar, y:Scalar) =
		indexOf(x, y) match {
			case Some(idx) =>
				array(idx)
			case None =>
				throw new RuntimeException("$this does not cover point $x, $y")
		}

	def containsPoint(x:Scalar, y:Scalar) =
		x >= offsetX && y >= offsetY &&
			x < offsetX + width && y < offsetY + height

	def update(v:Vec, value:A) { update(v.x, v.y, value) }
	def update(x:Scalar, y:Scalar, value:A) { indexOf(x, y) match {
		case Some(idx) => array(idx) = value
		case None => throw new RuntimeException("$this does not cover point $x, $y")
	} }

	/** Constrict a point to the valid range, then return the value there. */
	def getLimited(v:Vec): A = getLimited(v.x, v.y)
	def getLimited(x:Scalar, y:Scalar) = {
		val xi = ((x - offsetX) / elemSize).toInt.limitExcludingMax(0, matWidth)
		val yi = ((y - offsetY) / elemSize).toInt.limitExcludingMax(0, matHeight)
		array(xi + yi*matWidth)
	}

	/** If the point is in the matrix, returns Some(the value there).
		* Otherwise returns None.
		*/
	def get(x:Scalar, y:Scalar): Option[A] = indexOf(x, y).map(array)
	def get(v:Vec): Option[A] = get(v.x, v.y)

	def allNodes =
		array

	// @todo: remove
	def foreach(f:A => Unit) { array.foreach(f) }

	// @todo: remove
	def foreachWithPosition(f:(A, Vec) => Unit) {
		mutateMap { (a, vec) => f(a, vec); a }
	}

	def mutateMap(f:(A, Vec) => A) {
		var y: Scalar = offsetY
		var idx = 0
		while (idx < maxIndex) {
			var x: Scalar = offsetX
			val endOfRow = idx + matWidth
			while (idx < endOfRow) {
				array(idx) = f(array(idx), Vec(x, y))
				x += elemSize
				idx += 1
			}
			y += elemSize
		}
	}

	// @todo: remove
	def foreachInShape(shape:Shape)(f:A => Unit) {
		foreachInShapeWithPosition(shape) { (a, vec) => f(a) }
	}

	// @todo: remove
	def foreachInShapeWithPosition(shape:Shape)(f:(A, Vec) => Unit) {
		mutateMapInShapeWithPosition(shape) { (a, vec) => f(a, vec); a }
	}

	def nodesInShape(shape:Shape): Iterable[A] =
	{
		var xi0 = ((shape.left - offsetX) / elemSize).toInt
		var yi0 = ((shape.bottom - offsetY) / elemSize).toInt
		var xi1 = ((shape.right - offsetX) / elemSize).ceil
		var yi1 = ((shape.top - offsetY) / elemSize).ceil
		assert(xi0 <= xi1); require(yi0 <= yi1)

		if (xi0 < 0) {
			xi0 = 0
			if (xi1 < 0)
				return None
		}

		if (xi1 >= matWidth) {
			xi1 = matWidth - 1
			if (xi0 >= matWidth)
				return None
		}

		if (yi0 < 0) {
			yi0 = 0
			if (yi1 < 0)
				return None
		}

		if (yi1 >= matHeight) {
			yi1 = matHeight - 1
			if (yi0 >= matHeight)
				return None
		}

		// @todo: could be faster
		for (
			x <- xi0 to xi1;
			y <- yi0 to yi1)
			yield array(y * matWidth + x)
	}

	def mutateMapInShapeWithPosition(shape:Shape)(f:(A, Vec) => A) {
		var xi0 = ((shape.left - offsetX) / elemSize).toInt
		var yi0 = ((shape.bottom - offsetY) / elemSize).toInt
		var xi1 = ((shape.right - offsetX) / elemSize).ceil
		var yi1 = ((shape.top - offsetY) / elemSize).ceil
		require(xi0 <= xi1); require(yi0 <= yi1)

		if (xi0 < 0) {
			xi0 = 0
			if (xi1 < 0)
				return
		}

		if (xi1 >= matWidth) {
			xi1 = matWidth - 1
			if (xi0 >= matWidth)
				return
		}

		if (yi0 < 0) {
			yi0 = 0
			if (yi1 < 0)
				return
		}

		if (yi1 >= matHeight) {
			yi1 = matHeight - 1
			if (yi0 >= matHeight)
				return
		}

		mutateMapInSubMatrixWithPosition(xi0, yi0, xi1, yi1, f)
	}


	/** Iterates through every point in range [x0, y0] and [x1, y1], inclusive. */
	private def mutateMapInSubMatrixWithPosition(
		xi0:Int, yi0:Int, xi1:Int, yi1:Int,
		f:(A, Vec) => A)
	{
		require(xi0 >= 0 && yi0 >= 0 && xi1 < matWidth && yi1 < matHeight)

		val startX: Scalar = xi0 * elemSize + offsetX
		var y: Scalar = yi0 * elemSize + offsetY

		var idx = yi0 * matWidth + xi0
		val idxWidth = xi1 - xi0
		val idxSkip = (matWidth - idxWidth - 1)
		val startOfLastRow = yi1 * matWidth + xi0

		while (idx <= startOfLastRow) {
			var x = startX
			val lastIdxInRow = idx + idxWidth
			while (idx <= lastIdxInRow) {
				array(idx) = f(array(idx), Vec(x, y))
				x += elemSize
				idx += 1
			}
			y += elemSize
			idx += idxSkip
		}
	}

	/*def row(y:Int) = new Iterator[A] {
		var idx = y * width
		val end = idx + width
		def hasNext() = idx < end
		def next() = { val x = arr(idx); idx += 1; x }
	}

	def rows = new Iterator[Iterator[A]] {
		var y = 0
		def hasNext() = y < height
		def next() = { val x = row(y); y += 1; x }
	}*/

	/*override def toString = {
		val sb = new StringBuilder("Matrix(")
		for (row <- rows) {
			for (elt <- row) {
				sb ++= String.format("%8s", elt.toString)
			}
			sb += '\n'
		}
		sb.toString
	}*/
}


object OffsetSpaceMatrix {
	def apply[A : ClassTag](size:Vec, elemSize:Scalar)(filler: Vec => A) = {
		val osm = new OffsetSpaceMatrix[A](size.x, size.y, elemSize)
		osm.fill(filler)
		osm
	}
}

class OffsetSpaceMatrix[A : ClassTag] private(width:Scalar, height:Scalar, elemSize:Scalar)
												extends SpaceMatrix[A](width, height, elemSize) {
	private var _offsetX: Scalar = 0
	private var _offsetY: Scalar = 0
	override def offsetX = _offsetX
	override def offsetY = _offsetY
	def offset = Vec(_offsetX, _offsetY)
	def offset_=(v:Vec) { _offsetX = v.x; _offsetY = v.y }
}
