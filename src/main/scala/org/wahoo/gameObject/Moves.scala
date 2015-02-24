package org.wahoo
package gameObject

import math.Scalar,
			 shape.{Shape, Vec}

/** Trait for anything that moves.
  * If a HasNode is also Moves, its abNode may move it to a different node.
  */
trait Moves extends HasMutableShape {
	def StartVel =
		Vec.Zero

	private var _vel = StartVel

	override def vel = _vel

	def vel_=(v:Vec)
	{
		_vel = v
	}

	def velX =
		vel.x
	def velY =
		vel.y
	def velX_=(x:Scalar)
	{
		vel = vel.withX(x)
	}
	def velY_=(y:Scalar)
	{
		vel = vel.withY(y)
	}

	def accelerate(a:Vec)
	{
		vel += a
	}
	def accelerateLeft (ax:Scalar)
	{
		vel = vel.left (ax)
	}
	def accelerateRight(ax:Scalar)
	{
		vel = vel.right(ax)
	}
	def accelerateDown (ay:Scalar)
	{
		vel = vel.down(ay)
	}
	def accelerateUp(ay:Scalar)
	{
		vel = vel.up(ay)
	}

	def accelerateX(ax:Scalar)
	{
		accelerateRight(ax)
	}
	def accelerateY(ay:Scalar)
	{
		accelerateUp(ay)
	}

	def movingLeft =
		vel.leansLeft

	def movingRight =
		vel.leansRight

	def movingDown =
		vel.leansDown

	def movingUp =
		vel.leansUp

	def stopMovingX()
	{
		vel = vel.withXZero
	}
	def stopMovingY()
	{
		vel = vel.withYZero
	}
	def stopMoving ()
	{
		vel = Vec.Zero
	}
	def stopMovingLeft()
	{
		velX = velX.toNonNegative
	}
	def stopMovingRight()
	{
		velX = velX.toNonPositive
	}
	def stopMovingDown()
	{
		velY = velY.toNonNegative
	}
	def stopMovingUp()
	{
		velY = velY.toNonPositive
	}

	override def step()
	{
		super.step()
		move(_vel)
	}

	def decelerateX(amount:Scalar)
	{
		velX = velX.reduce(amount)
	}
	def decelerateY(amount:Scalar)
	{
		velY = velY.reduce(amount)
	}
	def decelerate(amount:Scalar)
	{
		vel = vel.reduceLength(amount)
	}

	def accelerateXWithBound(acc:Scalar, bound:Scalar)
	{
		velX = (velX + acc).limitSize(bound)
	}
	def accelerateYWithBound(acc:Scalar, bound:Scalar)
	{
		velY = (velY + acc).limitSize(bound)
	}
	def accelerateWithBound(acc:Vec, bound:Scalar)
	{
		vel = (vel + acc).withLengthLimit(bound)
	}

	def bounceLeft()
	{
		vel = vel withX -vel.x.abs
	}
	def bounceRight()
	{
		vel = vel withX vel.x.abs
	}
	def bounceDown()
	{
		vel = vel withY -vel.y.abs
	}
	def bounceUp()
	{
		vel = vel withY vel.y.abs
	}
}
