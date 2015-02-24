package org.wahoo
package gameObject
package facing

import shape.{BackOrForth, DownOrUp, LeftOrRight, Vec}

trait FacesBackOrForth
	extends InArena
{
	def StartFacing =
		BackOrForth.Back

	var facing:BackOrForth =
		StartFacing

	def face(f:BackOrForth)
	{
		facing = f
	}

	def faceBack()
	{
		face(BackOrForth.Back)
	}

	def faceForth()
	{
		face(BackOrForth.Forth)
	}

	def reverseFacing()
	{
		facing = facing.opposite
	}
}

trait FacesLeftOrRight
	extends InArena
{
	def StartFacing =
		LeftOrRight.Right

	var facing:LeftOrRight =
		StartFacing

	def face(s:InArena)
	{
		if (s.leftOf(this))
			faceLeft()
		else if (s.rightOf(this))
			faceRight()
	}

	def face(f:LeftOrRight)
	{
		facing = f
	}

	def faceLeft()
	{
		face(LeftOrRight.Left)
	}

	def faceRight()
	{
		face(LeftOrRight.Right)
	}

	def amFacing(f:LeftOrRight) =
		facing == f
	def facingLeft =
		amFacing(LeftOrRight.Left)
	def facingRight =
		amFacing(LeftOrRight.Right)

	def faceWith(v:Vec)
	{
		if (v.leansLeft)
			faceLeft()
		else if (v.leansRight)
			faceRight()
	}

	def reverseFacing()
	{
		facing = facing.opposite
	}
}

trait FacesWithVelX extends FacesLeftOrRight
{
	def vel: Vec

	override def step()
	{
		super.step()
		faceWith(vel)
	}
}

trait FlipsWithLeftOrRight
	extends Sprite
	with FacesLeftOrRight
{
	def facing: LeftOrRight

	override def flippedX =
		facing == LeftOrRight.Left
}

trait FlipsWithVelX
	extends FlipsWithLeftOrRight
	with FacesWithVelX

//trait FlipsWithVelY extends Sprite { override def flippedY = vel.leansDown }

//trait FlipsWithVel extends FlipsWithVelX with FlipsWithVelY
