package org.wahoo
package shape

//@todo: EDIT
import Anchor._, Side._

import
	gameObject.HasShape,
	math.Scalar

object Rect
{
	def centered(center:Vec, size:Vec): Rect =
		apply(center - size.half, size)

	def centered(center:Vec, size:Scalar): Rect =
		centered(center, Vec(size))

	def centered(center:Vec): Rect =
		centered(center, Vec.Zero)
	//def apply(left:Scalar, bottom:Scalar, width:Scalar, height:Scalar): Rect =
	//  apply(Vec(left, bottom), Vec(width, height))
	def apply(size:Vec): Rect =
		apply(Vec.Zero, size)
	//def apply(leftBottom:Vec, size:Vec) = Rect(leftBottom, size)

	def fromTo(fromX:Scalar, fromY:Scalar, toX:Scalar, toY:Scalar): Rect =
		fromTo(Vec(fromX, fromY), Vec(toX, toY))

	def fromTo(from:Vec, to:Vec) =
		Rect(from.x, to.x, from.y, to.y)

	//def cornerAndSize(left:Scalar, bottom:Scalar, width:Scalar, height:Scalar)

	val Zero = Rect(Vec.Zero, Vec.Zero)
	def zeroAt(pos:Vec) =
		apply(pos, Vec.Zero)

	def apply(leftBottom:Vec, size:Vec): Rect =
		apply(leftBottom.x, leftBottom.x + size.x, leftBottom.y, leftBottom.y + size.y)

	def containing(shapes:Shape*) =
	{
		require(shapes.nonEmpty)
		var left, bottom = Scalar.MaxValue
		var right, top = Scalar.MinValue

		for (shape <- shapes) {
			left = left min shape.left
			right = right max shape.right
			bottom = bottom min shape.bottom
			top = top max shape.top
		}

		val ans = Rect(left, right, bottom, top)
		assert(shapes forall { ans contains _ })
		ans
	}

	def containingObjects(hs: Seq[HasShape]): Rect =
		containing(hs.map(_.shape):_*)
}

case class Rect private[shape](
	left:Scalar, right:Scalar,
	bottom:Scalar, top:Scalar)
	extends Shape
{
	type MyType = Rect

	require(right >= left && top >= bottom)


	override def equals(obj:Any) =
		obj match {
			case r:Rect =>
				leftBottom == r.leftBottom && rightTop == r.rightTop
			case _ =>
				false
		}

	/*def expandedToContain(v:Vec) = {
		var newLeft = left
		var newRight = right
		var newBottom = bottom
		var newTop = top
		if (v.x < left) newLeft = v.x
		else if (v.x > right) newRight = v.x
		if (v.y < bottom) newBottom = v.y
		else if (v.y > top) newTop = v.y
		Rect(newLeft, newBottom, newRight - newLeft, newTop - newBottom)
	}*/

	// A new Rect relative to my topleft
	def subRect(dr:Vec, size:Vec) =
		Rect(leftBottom + dr, size)

	def isAbout(s:Shape) = s match {
		case r:Rect =>
			(leftBottom isAbout r.leftBottom) && (rightTop isAbout r.rightTop)
		case _ =>
			false
	}

	def width =
		right - left
	def height =
		top - bottom
	def size =
		Vec(width, height)

	def withLeft(x:Scalar) =
		Rect(x, right + x - left, bottom, top)
	def withRight(x:Scalar) =
		Rect(left + x - right, x, bottom, top)
	def withBottom(y:Scalar) =
		Rect(left, right, y, top + y - bottom)
	def withTop(y:Scalar) =
		Rect(left, right, bottom + y - top, y)

	def leftHalf =
		Rect(left, centerX, bottom, top)
	def rightHalf =
		Rect(centerX, right, bottom, top)
	def bottomHalf =
		Rect(left, right, bottom, centerY)
	def topHalf =
		Rect(left, right, centerY, top)

	private def movedX(dx:Scalar) =
		Rect(left + dx, right + dx, bottom, top)
	private def movedY(dy:Scalar) =
		Rect(left, right, bottom + dy, top + dy)

	def +(dr:Vec) =
		movedX(dr.x) movedY dr.y
	def -(dr:Vec) =
		movedX(-dr.x) movedY -dr.y

	def leftBottom =
		Vec(left, bottom)
	def rightBottom =
		Vec(right, bottom)
	def leftTop =
		Vec(left, top)
	def rightTop =
		Vec(right, top)
	def withLeftBottom(lb:Vec) =
		withLeft(lb.x) withBottom lb.y
	def withRightBottom(rb:Vec) =
		withRight(rb.x) withBottom rb.y
	def withLeftTop(lt:Vec) =
		withLeft(lt.x) withTop lt.y
	def withRightTop(rt:Vec) =
		withRight(rt.x) withTop rt.y

	//Remember, leftBottom + size = rightTop
	def centerX =
		left average right
	def centerY =
		bottom average top
	def center =
		Vec(centerX, centerY)
	def centerLeft =
		Vec(left, centerY)
	def centerRight =
		Vec(right, centerY)
	def centerBottom =
		Vec(centerX, bottom)
	def centerTop =
		Vec(centerX, top)
	def withCenterX(x:Scalar) =
		movedX(x - centerX)
	def withCenterY(y:Scalar) =
		movedY(y - centerY)
	def withCenter(c:Vec) =
		withCenterX(c.x) withCenterY c.y
	def withCenterLeft(cl:Vec) =
		withLeft(cl.x) withCenterY cl.y
	def withCenterRight(cr:Vec) =
		withRight(cr.x) withCenterY cr.y
	def withCenterBottom(cb:Vec) =
		withCenterX(cb.x) withBottom cb.y
	def withCenterTop(ct:Vec)  =
		withCenterX(ct.x) withTop ct.y

	def corners =
		Traversable(leftBottom, rightBottom, leftTop, rightTop)

	def withSize(newSize:Vec, anchor:Anchor) =
	{
		require(newSize.validSize)

		val w = newSize.x
		val h = newSize.y
		val dw = w - width
		val dh = h - height

		val leftCentered = left - dw.half
		val rightCentered = leftCentered + w
		val bottomCentered = bottom - dh.half
		val topCentered = bottomCentered + h

		val newRect = anchor match {
			case CenterLeft =>
				Rect(left, left + w,   bottomCentered, topCentered)
			case CenterRight =>
				Rect(right - w, right, bottomCentered, topCentered)
			case CenterBottom =>
				Rect(leftCentered, rightCentered, bottom, bottom + h)
			case CenterTop =>
				Rect(leftCentered, rightCentered, top - h, top)
			case Center =>
				Rect(leftCentered, rightCentered, bottomCentered, topCentered)
			case LeftBottom =>
				Rect(left, left + w, bottom, bottom + h)
			case RightBottom =>
				Rect(right - w, right, bottom, bottom + h)
			case LeftTop =>
				Rect(left, left + w, top - h, top)
			case RightTop =>
				Rect(right - w, right, top - h, top)
		}

		assert(newRect.size.isAbout(newSize),
			s"$this changed size to $newSize " +
			s"to produce $newRect with size ${newRect.size}! "+
			s"Anchor was $anchor")

		newRect
	}

	def collides(s:Shape) =
		s match {
			case v:Vec =>
				left <= v.x && v.x <= right &&
					bottom <= v.y && v.y <= top
			case r:Rect =>
				right >= r.left && top >= r.bottom &&
					left <= r.right && bottom <= r.top
			case c:Circle =>
				c collides this
		}

	//Given that another Rect is colliding with this, what side is it on?
	//Note: This method is not perfect if one is inside of the other;
	//but as this may happen, it does its best.
	def collideSide(s:Shape) =
		s match {
			case v:Vec =>
				val relX = (v.x - centerX) * height
				val relY = (v.y - centerY) * width
				if (relX.abs > relY.abs)
					// X difference is more relevant than Y difference
					if (relX.negative)
						Left
					else
						Right
				else
					if (relY.negative)
						Bottom
					else
						Top

			case r:Rect =>
				//Measure what sides of r are inside of me.
				val lIn = r.left >= left
				val rIn = r.right <= right
				val bIn = r.bottom >= bottom
				val tIn = r.top <= top

				if (lIn)
					if (rIn)
						if (tIn)
							if (bIn)
								//It's inside me! Hopefully rare.
								collideSide(r.center)
							else
								Bottom
						else
							if (bIn)
								Top
							else
								//lIn and rIn but top and bottom outside. Hopefully rare.
								collideSide(r.center)
					else
						//lIn but not rIn
						if (tIn)
							if (bIn)
								Right
							else
								//Other's left-top is in me.
								//Where is that relative to my right-bottom?
								if (right - r.left > r.top - bottom)
									Bottom
								else
									Right
						else
							//lIn but not rIn or tIn
							if (bIn)
								//Other's left-bottom is in me.
								//Where is that relative to my right-top?
								if (right - r.left > top - r.bottom)
									Top
								else
									Right
							else
								//only lIn
								Right
				else
					//not lIn
					if (rIn)
						if (tIn)
							if (bIn)
								Left
							else
								//Other's right-top is in me.
								//Where is that relative to my left-bottom?
								if (r.right - left > r.top - bottom)
									Bottom
								else
									Left
						else
							//rIn but not lIn or tIn
							if (bIn)
								//Other's right-bottom is in me.
								//Where is that relative to my left-top?
								if (r.right - left > top - r.bottom)
									Top
								else
									Left
							else
								//only rIn
								Left
					else
						//neither lIn nor rIn
						if (tIn)
							if (bIn)
								//tIn and bIn but left and right outside. Hopefully rare.
								collideSide(r.center)
							else
								//only tIn
								Bottom
						else
							//neither lIn nor rIn nor tIn
							if (bIn)
								Top
							else
								//None of its edges are inside me; I'm inside it!
								collideSide(r.center).opposite

			case c:Circle =>
				(c collideSide this).opposite
		}

	def contains(s:Shape) =
		s.left >= left && s.right <= right && s.bottom >= bottom && s.top <= top


	def fitsInside(s:Shape) =
		s match {
			case v:Vec  =>
				false
			case r:Rect =>
				size noLargerThan r.size
			case c:Circle =>
				???
		}

	/** Fails if can not move inside. */
	def movedInside(r:Rect) =
	{
		require(r.size aboutLargerThan size)

		val dl = r.left - left
		val (newL, newR) =
			if (dl.positive)
				(r.left, r.left + width/*right + dl*/)
			else {
				val dr = r.right - right
				if (dr.negative)
					(r.right - width, r.right)
				else
					(left, right)
			}

		val db = r.bottom - bottom
		val (newB, newT) =
			if (db.positive)
				(r.bottom, r.bottom + height)
			else {
				val dt = r.top - top
				if (dt.negative)
					(r.top - height, r.top)
				else
					(bottom, top)
			}

		val inside = Rect(newL, newR, newB, newT)
		assert(r.contains(this) == (inside == this))
		inside
	}

	def movedInside(c:Circle) =
		???
}
