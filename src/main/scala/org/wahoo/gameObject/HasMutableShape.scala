package org.wahoo
package gameObject

import math.Scalar,
			 shape.{Shape, Vec}

trait HasMutableShape
	extends HasShape
{

	private var _shape: Shape = Vec.Zero

	def shape =
		_shape

	/** This is override'd by InArenaAndMutableShape! */
	def shape_=(newShape:Shape)
	{
		_shape = newShape
	}

	def left_=(x:Scalar)
	{
		shape = shape withLeft x
	}
	def right_=(x:Scalar)
	{
		shape = shape withRight x
	}
	def bottom_=(y:Scalar)
	{
		shape = shape withBottom y
	}
	def top_=(y:Scalar)
	{
		shape = shape withTop y
	}
	def centerX_=(x:Scalar)
	{
		shape = shape withCenterX x
	}
	def centerY_=(y:Scalar)
	{
		shape = shape withCenterY y
	}
	def center_=(c:Vec)
	{
		shape = shape withCenter c
	}

	def moveInside(dr:Vec, s:Shape)
	{
		shape = (shape + dr) movedInside s
	}
	def moveInside(s:Shape)
	{
		shape = shape movedInside s
	}

	def move(dr:Vec)
	{
		shape = shape + dr
	}
}
