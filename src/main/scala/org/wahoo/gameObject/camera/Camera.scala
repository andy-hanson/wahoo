package org.wahoo
package gameObject
package camera

import
	math.implicits._,
	math.Scalar,
	shape.{Anchor, Rect, Shape, Vec},
	state.GameState

import org.lwjgl.opengl.GL11

trait Camera
	extends GameObject
	with HasShape
	with Unique
{
	def region:Shape

	def warpTo(pos:Vec)

	def follow(toFollow:InArena)

	/** Minimum bounds of this camera. */
	def boundary: Shape

	def applyProjection()
}

class SimpleCamera(val region:Shape)
	extends Camera
	with HasMutableShape
	with Unique
{
	shape = Rect(Vec.Zero, gameState.cameraSize)

	require(shape.fitsInside(region), s"Camera region $region too small for size $size")

	def FollowFactor: Scalar = 1 / 32.0
	//def MaxSpeed: Scalar = 8

	def warpTo(pos:Vec)
	{
		center = pos
	}

	var following: Option[InArena] = None

	override def step()
	{
		super.step()
		for (hn <- following)
			moveInside((shape to hn.shape) * FollowFactor, region)
	}

	def follow(toFollow:InArena)
	{
		following = Some(toFollow)

		if (Settings.InstantCamera)
			warpTo(toFollow.center)
	}

	/** Shape that bounds this camera.
		* Usually the plain rect; may be a RotatedRect if camera turns.
		*/
	def boundary = shape

	/** Tell OpenGL to translate everything to the camera's coordinates. */
	def applyProjection()
	{
		import GL11._
		glLoadIdentity() //Load identity matrix
		glOrtho(
			left.toFloat, right.toFloat,
			bottom.toFloat, top.toFloat,
			-1, 1)
	}
}
