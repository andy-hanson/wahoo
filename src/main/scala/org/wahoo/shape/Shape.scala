package org.wahoo
package shape

import math.Scalar

abstract class Shape
{
	/** This type must be equal to the type of the implementing class. */
	type MyType <: Shape

	/** Copy of this Shape shifted by a displacement. */
	def +(dr:Vec): MyType

	/** Copy of this Shape shifted by the opposite of a displacement. */
	def -(dr:Vec): MyType

	/** Whether this shape is nearly the same as another. */
	def isAbout(s:Shape): Boolean

	/** Width and height of the smallest rectangle surrounding this Shape. */
	def size: Vec

	/** Difference between rightmost x and leftmost x. */
	def width: Scalar

	/** Difference between topmost y and bottommost y. */
	def height: Scalar

	/** Copy of this Shape with the new size, with the anchor point held constant.
		* The shape should expand to reach the boundary of the new size, and is not
		* guaranteed to be contained in a Rect of that size.
		* And Vectors simply do nothing.
		*/
	def withSize(newSize:Vec, anchor:Anchor = Anchor.Center): MyType

	def shrunk(shrinkBy:Vec, anchor:Anchor) =
		expanded(-shrinkBy, anchor)

	def shrunk(shrinkBy:Scalar, anchor:Anchor = Anchor.Center) =
		expanded(-shrinkBy, anchor)

	def expanded(expandBy:Vec, anchor:Anchor) =
		withSize(size + expandBy, anchor)

	def expanded(expandBy:Scalar, anchor:Anchor = Anchor.Center): MyType =
		expanded(Vec(expandBy), anchor)

	def diameter =
		width max height

	def collidesWithXRangeExclusive(minX:Scalar, maxX:Scalar) =
	{
		require(minX <= maxX)
		right > minX && left < maxX
	}

	def collidesWithYRangeExclusive(minY:Scalar, maxY:Scalar) =
	{
		require(minY <= maxY)
		top > minY && bottom < maxY
	}

	def boundOnSide(s:Side) =
		s match {
			case Side.Left => left
			case Side.Right => right
			case Side.Bottom => bottom
			case Side.Top => top
		}

	/** Minimum x value. */
	def left: Scalar
	/** Maximum x value. */
	def right: Scalar
	/** Minimum y value. */
	def bottom: Scalar
	/** Maximum y value. */
	def top: Scalar
	/** Middle x value. */
	def centerX: Scalar
	/** Middle y value. */
	def centerY: Scalar

	/** (left, centerY) */
	def centerLeft: Vec
	/** (right, centerY) */
	def centerRight: Vec
	/** (centerX, bottom) */
	def centerBottom: Vec
	/** (centerX, top) */
	def centerTop: Vec
	/** (centerX, centerY) */
	def center: Vec
	/** (left, bottom) */
	def leftBottom: Vec
	/** (right, bottom) */
	def rightBottom: Vec
	/** (left, top) */
	def leftTop: Vec
	/** (right, top)  */
	def rightTop: Vec


	def withLeft(x:Scalar): MyType
	def withRight(x:Scalar): MyType
	def withBottom(y:Scalar): MyType
	def withTop(y:Scalar): MyType
	def withCenterX(x:Scalar): MyType
	def withCenterY(y:Scalar): MyType
	/** Copy of this shape with a different center. */
	def withCenter(c:Vec): MyType

	/** Vector from this Shape's center to another Shape's center.                 */
	def to(s:Shape) =
		s.center - center
	/** Square of the distance from this Shape's center to another Shape's center. */
	def distance2To(s:Shape) =
		to(s).length2
	/** Distance from this Shape's center to another Shape's center.               */
	def distanceTo(s:Shape) =
		to(s).length
	/** Unit vector to from this Shape's center to another Shape's center.         */
	def unitTo(s:Shape) =
		to(s).unit
	/** Angle from this Shape's center to another Shape's center.                  */
	def angleTo(s:Shape) =
		to(s).angle
	/** Vector in direction from this Shape to another with the given length.     */
	def toWithLength(s:Shape, len:Scalar) =
		to(s) withLength len

	/** Amount of time it takes to go directly to v at speed. */
	def timeTo(s:Shape, speed:Scalar) =
		(distanceTo(s) / speed).toInt

	def leftOf(s:Shape) =
		centerX < s.centerX
	def rightOf(s:Shape) =
		centerX > s.centerX
	def above(s:Shape) =
		centerY > s.centerY
	def below(s:Shape) =
		centerY < s.centerY

	/** Whether this Shape is touching another. */
	def collides(s:Shape): Boolean
	/** Whether another Shape does not extend beyond my bounds. */
	def contains(s:Shape): Boolean
	/** What side of me another Shape is on.
		* It is assumed that we collide.
		* Becomes inaccurate if one Shape is inside of another.
		* The Sides available are best used by Rects.
		* For other cases, consider using angleTo.
		*/
	def collideSide(s:Shape): Side


	def fitsInside(s:Shape): Boolean


	/** Copy of this Shape moved inside a Rect.   */
	def movedInside(r:Rect): MyType
	/** Copy of this Shape moved inside a Circle. */
	def movedInside(c:Circle): MyType

	/** Copy of this Shape inside of another. */
	def movedInside(s:Shape): MyType =
		s match {
			case v:Vec =>
				withCenter(v)
			case r:Rect =>
				val moved = movedInside(r)
				assert((r contains moved) || width > r.width || height > r.height)
				moved
			case c:Circle =>
				movedInside(c)
		}
}
