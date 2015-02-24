package org.wahoo
package shape

import
	math.implicits._,
	math.{random, randomAngle, Scalar}

case class Rotation(cos:Scalar, sin:Scalar)

object Vec
{
	def left(x:Scalar) =
		Vec(-x, 0)
	def right(x:Scalar) =
		Vec(x, 0)
	def up(y:Scalar) =
		Vec(0,  y)
	def down(y:Scalar) =
		Vec(0, -y)
	def leftDown(xy:Scalar) =
		Vec(-xy, -xy) * math.InvRoot2
	def rightDown(xy:Scalar) =
		Vec( xy, -xy) * math.InvRoot2
	def leftUp(xy:Scalar) =
		Vec(-xy,  xy) * math.InvRoot2
	def rightUp(xy:Scalar) =
		Vec( xy,  xy) * math.InvRoot2

	val Left =
		left(1)
	val Right =
		right(1)
	val Up =
		up(1)
	val Down =
		down(1)
	val LeftDown =
		leftDown (1)
	val RightDown =
		rightDown(1)
	val LeftUp =
		leftUp(1)
	val RightUp =
		rightUp(1)

	def apply(xy:Scalar): Vec =
		Vec(xy, xy)

	//def apply(xy:(Any, Any)): Vec = Vec(Scalar(xy._1), Scalar(xy._2))
	def apply(xy:(Int, Int)): Vec =
		Vec(xy._1, xy._2)
	def fromScalarPair(xy:(Scalar, Scalar)): Vec =
		Vec(xy._1, xy._2)

	def randomIn(s:Shape) =
		s match {
			case v:Vec =>
				v
			case r:Rect =>
				Vec(random(r.left, r.right), random(r.bottom, r.top))
			case c:Circle =>
				c.center + polar(c.radius, randomAngle())
		}

	val Zero =
		Vec(0, 0)

	def rotation(ang:Scalar): Rotation =
	{
		val (k, z) = ang.cosAndSin
		Rotation(k, z)
	}

	def randomOfLength(length:Scalar) =
		right(length).rotated(randomAngle())

	def randomUnit =
		randomOfLength(1)

	def polar(radius:Scalar, angle:Scalar) =
		Vec(radius * angle.cos, radius * angle.sin)
}

case class Vec(x:Scalar, y:Scalar) extends Shape
{
	type MyType = Vec

	override def equals(obj:Any) = obj match {
		case v:Vec =>
			x == v.x && y == v.y
		case _ =>
			false
	}

	def rotated(amnt:Scalar): Vec =
		rotated(Vec.rotation(amnt))

	def rotated(rot:Rotation) =
		Vec(x * rot.cos - y * rot.sin, x * rot.sin + y * rot.cos)

	def aboutSmallerThan(v:Vec) =
		(x aboutLessThan v.x) && (y aboutLessThan v.y)

	def aboutLargerThan (v:Vec) =
		(x aboutMoreThan v.x) && (y aboutMoreThan v.y)

	def noSmallerThan(v:Vec) =
		x >= v.x && y >= v.y

	def noLargerThan (v:Vec) =
		x <= v.x && y <= v.y

	def fitsInside(s:Shape) =
		this == s

	def size =
		Vec.Zero
	def width =
		0
	def height =
		0
	def validSize =
		x >= 0 && y >= 0
	def withSize(s:Vec, anchor:Anchor) =
		Vec(x, y)

	def isAbout(s:Shape) =
		s match {
			case v:Vec => (this - v).isAboutZero
			case _ => false
		}
	def isAboutZero =
		x.isAboutZero && y.isAboutZero
	def notAboutZero =
		!isAboutZero
	def isAboutCompassDirection =
		x.abs isAbout y.abs
	def notAboutCompassDirection =
		!isAboutCompassDirection


	def +(dr:Vec) =
		Vec(x + dr.x, y + dr.y)

	def -(dr:Vec) =
		Vec(x - dr.x, y - dr.y)

	def to(v:Vec) =
		v - this

	def nonZero =
		this != Vec.Zero

	def withX(newX:Scalar) =
		Vec(newX, y)
	def withXZero =
		Vec(0, y)
	def withY(newY:Scalar) =
		Vec(x, newY)
	def withYZero =
		Vec(x, 0)

	def left =
		x
	def right =
		x
	def bottom =
		y
	def top =
		y
	def centerX =
		x
	def centerY =
		y

	def centerLeft =
		this
	def centerRight =
		this
	def centerBottom =
		this
	def centerTop =
		this
	def center =
		this
	def leftBottom =
		this
	def rightBottom =
		this
	def leftTop =
		this
	def rightTop =
		this

	def withLeft(left:Scalar) =
		Vec(left , y)
	def withRight(right:Scalar) =
		Vec(right, y)
	def withBottom(bottom:Scalar) =
		Vec(x, bottom)
	def withTop(top:Scalar) =
		Vec(x, top)
	def withCenterX(cx:Scalar) =
		Vec(cx, y)
	def withCenterY(cy:Scalar) =
		Vec(x, cy)
	def withCenter(v:Vec) =
		v

	def leansLeft =
		x.negative
	def leansRight =
		x.positive
	def leansDown  =
		y.negative
	def leansUp =
		y.positive
	def xMore =
		x.abs > y.abs
	def yMore =
		y.abs > x.abs
	def minXY =
		x min y
	def maxXY =
		x max y

	def flippedX =
		withX(-x)
	def flippedY =
		withY(-y)

	def dot(v:Vec) =
		x * v.x + y * v.y

	def project(v:Vec) =
		v * (dot(v) / v.length2)
	def perp(v:Vec) =
		this - project(v)

	/** Invert my component perpendicular to v.
		*/
	def reflect(v:Vec) =
		project(v).twice - this

	def angle =
		math atan2 (y, x)

	def angleDifference(v:Vec) =
		(dot(v) / (length2 * v.length2).sqrt).safeACos

	def angleTo(v:Vec) =
		to(v).angle

	/* Make sure my component in v's direction is positive. */
	def reflectTo(v:Vec) =
	{
		val dott = dot(v)
		if (dott.nonNegative)
			this
		else {
			val along = v * (dott / v.length2)
			this - along * 2
		}
	}

	def left(dx:Scalar) =
		Vec(x - dx, y)
	def right(dx:Scalar) =
		Vec(x + dx, y)
	def down(dy:Scalar) =
		Vec(x, y - dy)
	def up(dy:Scalar) =
		Vec(x, y + dy)

	def half =
		Vec(x.half, y.half)
	def halfX =
		withX(x.half)
	def halfY =
		withY(y.half)

	def twice =
		Vec(x.twice, y.twice)
	def twiceX =
		withX(x.twice)
	def twiceY =
		withY(y.twice)

	def scaleWith(v:Vec) =
		Vec(x * v.x, y * v.y)

	def /(k:Scalar) =
		Vec(x / k, y / k)

	def *(k:Scalar) =
		Vec(x * k, y * k)

	def unary_- =
		Vec(-x, -y)

	def length2 =
		x.square + y.square

	def length:Scalar =
		length2.sqrt

	/** Converts to a vector in same direction with length 1.
		* Fails for Vec.Zero. For that, see unitOrZero.
		*/
	def unit =
		Vec(x / length, y / length)

	/** Returns a unit vector if it exists, else zero. */
	def unitOrZero =
		if (nonZero)
			unit
		else
			Vec.Zero

	def withLength(len:Scalar) =
	{
		require(length > 0)
		this * (len / length)
	}

	def withLengthLimit(maxLength:Scalar) =
	{
		if (length2 > maxLength.square)
			this * (maxLength / length)
		else
			this
	}

	/** Reduces my length by amount, returning Zero if shorter than amount. */
	def reduceLength(amount:Scalar) =
	{
		val newLength =
			length - amount
		if (newLength <= 0)
			Vec.Zero
		else
			withLength(newLength)
	}


	def collides(s:Shape) = s match
	{
		case v:Vec =>
			v == this
		case _ =>
			s collides this
	}

	/** No guarantee if this == v
		* If x.abs == y.abs:
		*   Favors returning Top and Bottom over Left and Right
		*/
	def collideSide(s:Shape) =
		s match {
			case v:Vec =>
				val rel = to(v)
				if (rel.xMore)
					if (rel.leansLeft)
						Side.Left
					else
						Side.Right
				else
					if (rel.leansUp)
						Side.Top
					else
						Side.Bottom
			case _ =>
				s.collideSide(this).opposite
		}

	def contains(s:Shape) =
		false
		// Vec == is never gonna happen...
		/*s match {
			case v:Vec =>
				v == this
			case _ =>
				false
		}*/

	def movedInside(r:Rect) =
		Vec(xInside(r), yInside(r))

	def xInside(r:Rect) =
		if (x < r.left)
			r.left
		else if (x > r.right)
			r.right
		else x

	def yInside(r:Rect) =
		if (y < r.bottom)
			r.bottom
		else if (y > r.top)
			r.top
		else y

	// @todo
	def movedInside(c:Circle) =
		???


	/** String of this Vec with values truncated to ints. */
	def intString =
		f"Vec(${x.toInt}%5d,${y.toInt}%5d)"
}
