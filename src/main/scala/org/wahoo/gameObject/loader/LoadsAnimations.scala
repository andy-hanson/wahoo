package org.wahoo
package gameObject
package loader

import
	logic.AccessesUniques,
	math.Scalar,
	math.implicits._,
	resource.{AnimationData, Animation},
	shape.{Shape, Vec},
	util.packageToDirectoryName

trait LoadsAnimations
	extends AccessesUniques
{
	def AnimationDirectoryName =
		getClass.getSimpleName

	def AnimationFullDirectory =
		s"${packageToDirectoryName(getClass)}/$AnimationDirectoryName"

	def isTest =
		false

	def lit =
		false

	def animationData(
		name: String,
		frames: Int = 1,
		speed: Int = 30,
		scale: Scalar  = 1.0,
		loop: Boolean = true,
		shapeShrink: (Scalar, Scalar) = (0.0, 0.0),
		synchronized: Boolean = false) =
		AnimationData(
			AnimationFullDirectory,
			name, frames, speed, scale, loop,
			shapeShrink, synchronized, isTest)

	def ani(
		name: String,
		frames: Int     = 1,
		speed: Int     = 30,
		loop: Boolean = true,
		scale: Scalar  = 1.0,
		shapeShrink: (Scalar, Scalar) = (0.0, 0.0),
		synchronized: Boolean = false) =
		animationData(name, frames, speed, scale, loop, shapeShrink, synchronized)

	def animation(
		name: String,
		frames: Int     = 1,
		speed: Int     = 30,
		scale: Scalar  = 1.0,
		loop : Boolean = true,
		shapeShrink: (Scalar, Scalar) = (0.0, 0.0)) =
		Animation(animationData(name, frames, speed, scale, loop, shapeShrink))

	def renderAnimation(animation:Animation, x:Scalar, y:Scalar)
	{
		renderAnimation(animation, x, y, false, false)
	}

	def renderAnimation(
		animation:Animation, x:Scalar, y:Scalar,
		flipX:Boolean, flipY:Boolean)
	{
		render animation (animation, x, y, lit, flipX, flipY)
	}

	def renderAnimation(
		animation:Animation, s:Shape,
		flipX:Boolean = false, flipY:Boolean = false)
	{
		render animation (animation, s, lit, flipX, flipY)
	}

	def animationFromData(data:AnimationData): Animation =
		if (data.synchronized)
			the[SynchronizedAnimationMaster] get data
		else
			Animation(data)

	def AniNone =
		resource.NoAnimationData
}
