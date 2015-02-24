package org.wahoo
package state
package spacialStore

import collection.ArrayBag,
	gameObject.InArena,
	math.Scalar,
	shape.{Shape, Vec}

import scala.reflect.ClassTag

object SpacialStore
{
	def apply(size:Vec, minNodeSize:Scalar, maxNodeSize:Scalar) =
		new LooseQuadTree(size, minNodeSize, maxNodeSize)
}

abstract class SpacialStore
{
	def add(o:InArena)

	def remove(o:InArena)

	def allDrawObjectsColliding(s:Shape): Traversable[BufferedIterator[InArena]]

	def callAllStepsColliding(s:Shape)

	def drawAllColliding(s:Shape)

	def allCollidingOfType[A >: Null <: InArena : ClassTag](s:Shape): Traversable[A] =
	{
		val bag = ArrayBag.empty[A]
		eachCollidingOfType[A](s) { bag += _ }
		bag
	}

	def eachCollidingOfType[A <: InArena : ClassTag](s:Shape)(f:A => Unit)

	def drawNodesColliding(s:Shape)

	def mayNeedToMove(o:InArena)

	def containsShape(s:Shape): Boolean
}
