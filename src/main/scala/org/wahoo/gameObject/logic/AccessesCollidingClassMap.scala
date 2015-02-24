package org.wahoo
package gameObject
package logic

import math.Scalar,
			 shape.{Shape, Side},
			 state.spacialStore.SpacialStore

import scala.reflect.ClassTag

trait AccessesCollidingClassMap
	extends AccessesClassMap
{
	def collides(o:InArena): Boolean

	def collideSide(o:InArena): Side

	def spacialStore: SpacialStore

	def shape: Shape


	def colliding[A >: Null <: InArena : ClassTag] =
		spacialStore.allCollidingOfType[A](shape)

	def collidingWhen[A >: Null <: InArena : ClassTag](f:A => Boolean): Traversable[A] =
		colliding[A] filter f

	/** Iterates through every object of type A that may be colliding with this,
		* (that means any object in this object's node or any of its parents)
		* and checks for collision.
		* For each collision, calls f.
		* @todo use spacialStore.collidingOfType and `for`.
		*/
	def eachColliding[A <: InArena : ClassTag](f:A => Unit)
	{
		 spacialStore.eachCollidingOfType[A](shape) { obj =>
		 	if (obj ne this)
		 		f(obj)
		 }
	}

	/** Iterates through every colliding object satisfying some predicate. */
	// @todo use spacialStore.collidingOfType and `for`.
	def eachCollidingWhen[A <: InArena : ClassTag](filter:A => Boolean)(f:A => Unit)
	{
		spacialStore.eachCollidingOfType[A](shape) { obj =>
			if (filter(obj) && (obj ne this)) f(obj)
		}
	}

	/** Map of side to objects on that side.
		* Map will not contain side as a key if there are no objects on that side.
		*/
	def collidingBySideWhen[A >: Null <: InArena : ClassTag](
		f:A => Boolean)
		: Map[Side, Seq[A]] =
		collidingWhen[A](f) groupBy { collideSide(_) } mapValues { _.toSeq }

	/** Map of side to the closest object on that side.
	  * Map will not contain side as a key if there are no objects on that side.
	  * Also filters out false positive collisions.
	  */
	def closestCollidingBySideWhen[A >: Null <: InArena : ClassTag](
		f:A => Boolean)
		: Map[Side, A] =
	{
		val bySide =
			collidingBySideWhen(f)

		val onLefts: Option[Seq[A]] =
			bySide get Side.Left map { _ sortBy { -_.right } }
		val onRights =
			bySide get Side.Right map { _.sortBy { _.left } }
		val onBottoms =
			bySide get Side.Bottom map { _.sortBy { -_.top } }
		val onTops =
			bySide get Side.Top map { _.sortBy { _.bottom } }

		val leftBound =
			onLefts map { _.head.right } getOrElse Scalar.MinValue
		val rightBound =
			onRights map { _.head.left } getOrElse Scalar.MaxValue
		val bottomBound =
			onBottoms map { _.head.top } getOrElse Scalar.MinValue
		val topBound =
			onTops map { _.head.bottom } getOrElse Scalar.MaxValue

		def okLeftRight(a:A) =
			a.shape collidesWithYRangeExclusive (bottomBound, topBound)
		def okBottomTop(a:A) =
			a.shape collidesWithXRangeExclusive (leftBound, rightBound)

		val sideToObject: Map[Side, Option[A]] =
			Map(
				Side.Left -> (onLefts flatMap { _.find { okLeftRight(_) } }),
				Side.Right -> (onRights flatMap { _.find { okLeftRight(_) } }),
				Side.Bottom -> (onBottoms flatMap { _.find { okBottomTop(_) } }),
				Side.Top -> (onTops flatMap { _.find { okBottomTop(_) } }))

		val onlySidesWithColliding =
			sideToObject filter { case (side, optionSeq) =>
				optionSeq.nonEmpty
			}

		onlySidesWithColliding mapValues { _.get }
	}
}
