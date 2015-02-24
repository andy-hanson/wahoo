package org.wahoo
package shape

import
	math.Scalar,
	math.implicits._

sealed abstract class BackOrForth
{
	def opposite: BackOrForth
	def sign: Int
}

object BackOrForth
{
	case object Back extends BackOrForth
	{
		def opposite = Forth
		def sign = -1
	}

	case object Forth extends BackOrForth
	{
		def opposite = Back
		def sign = 1
	}
}

sealed abstract class LeftOrRight
{
	def opposite: LeftOrRight

	def sign: Int

	def toX(magnitude:Scalar): Scalar

	def vec: Vec

	def isInDirection(v:Vec): Boolean

	def isOpposite(v:Vec): Boolean
}

object LeftOrRight
{
	case object Left extends LeftOrRight
	{
		def opposite = Right
		def sign = -1
		def toX(magnitude:Scalar) = -magnitude
		def vec = Vec.Left
		def isInDirection(v:Vec) = v.leansLeft
		def isOpposite(v:Vec) = v.leansRight
	}

	case object Right extends LeftOrRight
	{
		def opposite = Left
		def sign = 1
		def toX(magnitude:Scalar) = magnitude
		def vec = Vec.Right
		def isInDirection(v:Vec) = v.leansRight
		def isOpposite(v:Vec) = v.leansLeft
	}
}

sealed abstract class DownOrUp
{
	def opposite: DownOrUp

	def sign: Int

	def toY(magnitude:Scalar): Scalar
}

object DownOrUp
{
	case object Down extends DownOrUp
	{
		def opposite = Up
		def sign = -1
		def toY(magnitude:Scalar) =
			-magnitude
	}

	case object Up extends DownOrUp
	{
		def opposite = Down
		def sign = 1
		def toY(magnitude:Scalar) =
			magnitude
	}
}
