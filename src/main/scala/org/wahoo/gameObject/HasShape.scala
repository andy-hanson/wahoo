package org.wahoo
package gameObject

import
	color.Color,
	math.Scalar,
	math.implicits._,
	shape.{Shape, Side, Vec}

/** An object that has a Shape.
	* For an object whose shape refers to its position in the arena, see InArena.
	*/
trait HasShape extends GameObject {
	def shape: Shape

	/** Current velocity. */
	def vel =
		Vec.Zero

	def left =
		shape.left
	def right =
		shape.right
	def bottom =
		shape.bottom
	def top =
		shape.top
	def centerX =
		shape.centerX
	def centerY =
		shape.centerY
	def leftBottom =
		shape.leftBottom
	def leftTop =
		shape.leftTop
	def rightBottom =
		shape.rightBottom
	def rightTop =
		shape.rightTop
	def center =
		shape.center
	def width =
		shape.width
	def height =
		shape.height
	def size =
		shape.size

	def drawShapeWidth: Scalar =
		4

	def drawShapeColor =
		Color.GreenGrass

	def drawShape()
	{
		render shape (shape, drawShapeWidth, drawShapeColor)
	}
}
