package org.wahoo
package shape

import Anchor._, Side._ //@todo: EDIT

import math.Scalar

/** Factory object for Circles. */
object Circle
{
	/** Create a circle by constructing a center Vec. */
	def apply(cx:Scalar, cy:Scalar, rad:Scalar): Circle =
		apply(Vec(cx, cy), rad)
}

/** The set of all points that are a distance of rad or less from center. */
case class Circle(center:Vec, rad:Scalar)
	extends Shape
{
	type MyType = Circle

	def radius =
		rad

	override def equals(obj:Any) =
		obj match {
			case c:Circle =>
				this.center == c.center && this.rad == c.rad
			case _ =>
				false
		}

	def fitsInside(s:Shape) = s match {
		case v:Vec =>
			false
		case r:Rect =>
			rect fitsInside r
		case c:Circle =>
			rad < c.rad
	}

	/** Square of the radius. */
	def rad2 =
		rad.square

	def centerX =
		center.x

	def centerY =
		center.y

	def left(dx:Scalar) =
		Circle(center left dx, rad)

	def right(dx:Scalar) =
		Circle(center right dx, rad)

	def down(dy:Scalar) =
		Circle(center down dy, rad)

	def up(dy:Scalar) =
		Circle(center up dy, rad)

	def isAbout(s:Shape) =
		s match {
			case c:Circle =>
				(center isAbout c.center) && (rad isAbout c.rad)
			case _ => false
		}

	def size = Vec(width, height)
	def halfSize = Vec(rad, rad)
	def width  = rad.twice
	def height = rad.twice
	def rect = Rect(leftBottom, size)

	def left   = centerX - rad
	def right  = centerX + rad
	def bottom = centerY - rad
	def top    = centerY + rad

	def centerLeft   = center.left(rad)
	def centerRight  = center.right(rad)
	def centerBottom = center.down(rad)
	def centerTop    = center.up(rad)
	def leftBottom   = center - halfSize
	def rightBottom  = Vec(right, bottom)
	def leftTop      = Vec(left, top)
	def rightTop     = center + halfSize

	def withLeft  (newLeft:Scalar)   =  Circle(newLeft + rad,  centerY,         rad)
	def withRight (newRight:Scalar)  =  Circle(newRight - rad, centerY,         rad)
	def withBottom(newBottom:Scalar) =  Circle(centerX,        newBottom + rad, rad)
	def withTop   (newTop:Scalar)    =  Circle(centerX,        newTop - rad,    rad)
	def withCenterX(x:Scalar) = Circle(center.withX(x), rad)
	def withCenterY(y:Scalar) = Circle(center.withY(y), rad)
	/** Copy of this Circle with a different center. */
	def withCenter(c:Vec) = Circle(c, rad)

	def +(dr:Vec) = Circle(center + dr, rad)
	def -(dr:Vec) = Circle(center - dr, rad)

	/* Returns the smallest circle containing the middle of each edge of the new size rect. */
	def withSize(newSize:Vec, anchor:Anchor) = withRadius(newSize.maxXY.half, anchor)

	def withRadius(newRadius:Scalar, anchor:Anchor) = {
		val dr = newRadius - rad

		val newCenter = anchor match {
			case CenterLeft   => center.right(dr)
			case CenterRight  => center.left(dr)
			case CenterBottom => center.up(dr)
			case CenterTop    => center.down(dr)
			case Center       => center
			case LeftBottom   => Vec(centerX + dr, centerY + dr)
			case RightBottom  => Vec(centerX - dr, centerY + dr)
			case LeftTop      => Vec(centerX + dr, centerY - dr)
			case RightTop     => Vec(centerX - dr, centerY - dr)
		}

		Circle(newCenter, newRadius)
	}

	def collides(s:Shape) = s match {
		case v:Vec    => (v - center).length2 < rad2
		case r:Rect   => rect.collides(r)
		case c:Circle =>
			val rads = c.rad + rad
			distance2To(c) <= rads * rads
	}

	def collideSide(s:Shape) = center.collideSide(s)

	def contains(s:Shape) = s match {
		case v:Vec    => collides(v)
		case r:Rect   => r.corners.forall(contains(_))
		case c:Circle => distanceTo(c) + c.rad < rad
	}

	def movedInside(r:Rect) = Circle(xInside(r), yInside(r), rad)
	protected def xInside(r:Rect) =
		if (left < r.left) r.left + rad
		else if (right > r.right) r.right - rad
		else centerX
	protected def yInside(r:Rect) =
		if (bottom < r.bottom) r.bottom + rad
		else if (top > r.top) r.top - rad
		else centerY

	def movedInside(c:Circle) = throw new RuntimeException("Write me!")
}
