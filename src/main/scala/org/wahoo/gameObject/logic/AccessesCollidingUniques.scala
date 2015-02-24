package org.wahoo
package gameObject
package logic

import scala.reflect.ClassTag

trait AccessesCollidingUniques
	extends AccessesUniques
{
	def collides(o:InArena): Boolean

	def maybeTheColliding[A <: Unique with InArena : ClassTag]: Option[A] =
		maybeThe[A] filter { collides(_) }
}
