package org.wahoo
package gameObject

import shape.{Anchor, Rect, Shape, Vec},
	resource.{Animation, AnimationData}

// Class adds an image and associated rect.
// Rect resizes about the anchor. Default anchor is Center.
abstract class Sprite(startPos:Vec)
	extends GameObject
	with HasMutableShape
{
	def StartAnimation =
		AniIdle

	private var _animation: Animation = null

	def animation =
		_animation

	override def start()
	{
		super.start()
		setAnimation(StartAnimation)
	}

	def initSize =
		animation.size

	def image =
		animation.frame

	def anchor: Anchor =
		Anchor.Center

	shape =
		Rect centered startPos

	/** Idle animation must be defined. */
	def AniIdle: AnimationData

	override def step()
	{
		super.step()
		animation.step()
		if (animation.changesSize)
			resize()
	}

	def flippedX =
		false

	def flippedY =
		false

	override def draw()
	{
		super.draw()
		if (Settings.DrawShapes)
			drawShape()
		renderAnimation(animation, shape, flippedX, flippedY)
	}

	/** Start using a new animation.
	  * Passing the same animation to this function will have the effect of resetting it.
	  * @todo or maybe make ani() return AnimationData instead.
	  */
	def setAnimation(newAnimation:AnimationData)
	{
		_animation = animationFromData(newAnimation)
		resize()
	}

	def isUsingAnimation (data:AnimationData) =
		animation.data == data

	def mustHaveAnimation(data:AnimationData)
	{
		assert(isUsingAnimation(data),
			s"Expected animation $data, but had ${animation.data}")
	}

	def mustChangeAnimation(from:AnimationData, to:AnimationData)
	{
		require(from != to)
		mustHaveAnimation(from)
		setAnimation(to)
	}

	private def resize()
	{
		resize(animation.size)
	}

	def resize(newSize:Vec)
	{
		shape = shape withSize (newSize, anchor)
	}
}
