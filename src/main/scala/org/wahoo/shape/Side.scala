package org.wahoo
package shape

object Side {
	case object Left extends Side
	{
		def opposite = Right
	}

	case object Right extends Side
	{
		def opposite = Left
	}

	case object Bottom extends Side
	{
		def opposite = Top
	}

	case object Top extends Side
	{
		def opposite = Bottom
	}

	val All = Seq(Left, Right, Bottom, Top)
}

sealed abstract class Side
{
	def opposite: Side
}
